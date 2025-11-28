module Language.CCS.Lexer.Pipeline
  ( pipeline
  , pipelineFrom
  , decode
  , lines
  , linesFrom
  ) where

import Prelude hiding (lex, lines)

import Data.Char (chr, ord, isOctDigit, isDigit, isHexDigit, toLower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Language.CCS.Error (placeholder, internalError, unwrapOrPanic_)
import Language.Location (Span, mkSpan, spanFromPos, Pos, startPos, incLine, incCol)
import Language.CCS.Lexer.Morpheme (Token(..), annotation)
import Language.CCS.Lexer.Morpheme (QuoteType(..), EolType(..))
import Language.CCS.Lexer.Morpheme (PunctuationType(..), BracketType(..))
import Language.CCS.Lexer.Morpheme (Sign(..), Radix(..))

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding.Error as Codec
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Codec

pipeline :: LBS.ByteString -> [Token Span]
pipeline = pipelineFrom startPos

pipelineFrom :: Pos -> LBS.ByteString -> [Token Span]
pipelineFrom pos0 bytes = bytes
  & decode
  & linesFrom pos0
  & lex StdLex

-- NOTE should this helper be here?
advInLine :: Pos -> Text -> Pos
advInLine pos str = T.foldl' (\p _ -> incCol p) pos str

--------------------
------ Decode ------
--------------------

decode :: LBS.ByteString -> LT.Text
decode bytes = placeholder
  $ bytes
  & Codec.decodeUtf8With Codec.lenientDecode -- invalid bytes are replaced with U+FFFD (replacement character), so downstream from this placeholder can't have a byte offset

-------------------------
------ Split Lines ------
-------------------------

data Line = Line
  { content :: Text
  , loc :: Span
  , eol :: EolType
  }

lines :: LT.Text -> [Line]
lines = linesFrom startPos

linesFrom :: Pos -> LT.Text -> [Line]
linesFrom _ LT.Empty = [] -- NOTE this means a zero-byte file will have an empty list of lines; anything else will have at least one line (possibly blank ofc)
linesFrom l str = Line
  { content
  , loc = unwrapOrPanic_ $ mkSpan l r
  , eol
  } : maybe [] (linesFrom nextLine) rest
  where
  (lazyContent, eol, rest) = case LT.break (\c -> c == '\n' || c == '\r') str of
    (pre,                   LT.Empty) -> (pre, Eof,  Nothing)
    (pre,            '\n' LT.:< post) -> (pre, LF,   Just post)
    (pre, '\r' LT.:< '\n' LT.:< post) -> (pre, CRLF, Just post)
    (pre, '\r' LT.:<            post) -> (pre, CR,   Just post)
    _ -> internalError "expected an end of line sequence after preaking on `\\n|\\r`"
  content = LT.toStrict lazyContent
  r = T.foldl' (\pos _ -> incCol pos) l content
  nextLine = incLine l

----------------------------
------ Coverage Lexer ------
----------------------------

data LexMode
  = StdLex
  | NumLex Radix
  | SqLex
  | DqLex
  | TqLex Text

-- > ┌─────┐
-- > │ std │◀───────────────────────────────────────────────┐
-- > └─────┘                                                │
-- >    │                                                   │
-- >    │                                                   │
-- >    │──▶ # ──▶ .* ─────────────────────▶ Comment ──────▶│
-- >    │                                                   │
-- >    │                                                   │
-- >    │──▶ [\t ] ──▶ [\t ]* ─────────────▶ Lws ──────────▶│
-- >    │                                                   │
-- >    │                                                   │
-- >    │──▶ [()[\]{},.:;\\] ──────────────▶ Punctuation ──▶│
-- >    │                                                   │
-- >    │                                                   │
-- >    │──▶ [:id0:] ────────┐                              │
-- >    │                    │─▶ [:id:]* ──▶ Identifier ───▶│
-- >    │──▶ [+-](?![^0-9]) ─┘
-- >    │
-- >    │
-- >    │──▶ [+-](?:[0-9]) ──▶ Sign ──┐                  ┌─────┐
-- >    │                             │─────────────────▶│ num │
-- >    │──▶ (?:0-9]) ────────────────┘                  └─────┘
-- >    │
-- >    │                                                ┌───────┐
-- >    │──▶ ' ────────────────────▶ OpenStr ───────────▶│ sqStr │
-- >    │                                                └───────┘
-- >    │                                                ┌───────┐
-- >    │──▶ "(?!"")|` ────────────▶ OpenStr ───────────▶│ dqstr │
-- >    │                                                └───────┘
-- >    │
-- >    │──▶ """ ──▶ "*[a-zA-Z]* ──▶ OpenStr
-- >                                    │
-- >                                    │──▶ [\t ]+ ─▶ Lws
-- >                                    │               │
-- >                                    │◀──────────────┘
-- >                                    │
-- >                                    │──▶ #.* ──▶ Comment
-- >                                    │               │
-- >                                    │◀──────────────┘
-- >                                    │                ┌───────┐
-- >                                    └──▶ $ ─────────▶│ tqStr │
-- >                                                     └───────┘

stdLex :: Pos -> (Char, Text) -> LexerStep
stdLex l (c, cs) = case classify c of
  -- comments
  Hash -> (Comment l (c T.:< cs), Left [], StdLex)
  -- linear whitespace
  Lws ->
    let (lws, r, rest) = _takeClasses (==Lws) (l, c, cs)
     in (Whitespace l lws, Right (r, rest), StdLex)
  -- punctuation
  Punct pTy -> (Punctuation l pTy, Right (incCol l, cs), StdLex)
  -- symbols
  Id0 ->
    let (sym, r, rest) = _takeClasses (`elem` [Id0, Sgn, Num]) (l, c, cs)
     in (Symbol l sym, Right (r, rest), StdLex)
  Sgn -> case cs of
    T.Empty -> (Symbol l (T.singleton c), Left [], StdLex)
    c' T.:< _ | classify c' /= Num ->
      let (sym, r, rest) = _takeClasses (`elem` [Id0, Sgn, Num]) (l, c, cs)
      in (Symbol l sym, Right (r, rest), StdLex)
  -- numbers
      | otherwise ->
        let sign = unwrapOrPanic_ $ parseSign c
         in (Sign l sign, Right (incCol l, cs), NumLex Base10)
  Num -> numLex Base10 l (c, cs)
  -- inline strings
  SQuote ->
    (Quote l SqlQuote, Right (incCol l, cs), SqLex)
  BtQuote ->
    (Quote l Backtick, Right (incCol l, cs), DqLex)
  DQuote | not $ "\"\"" `T.isPrefixOf` cs ->
    (Quote l DblQuote, Right (incCol l, cs), DqLex)
  -- multiline strings
         | otherwise ->
    let (quotes, nameL, afterQuotes) = _takeClasses (==DQuote) (l, c, cs)
        (name, lwsL, afterName) = _spanClasses (==Id0) (nameL, afterQuotes) -- NOTE I've over-allowed the quote name, it should match [a-zA-Z], not [:id0:]
        (lws, illL, afterLws) = _spanClasses (==Lws) (lwsL, afterName)
        (ill, commentL, afterIll) = _spanClasses (/=Hash) (illL, afterLws)
        -- construct tokens
        delim = quotes <> name
        extraTokens = catMaybes
          [ if T.null lws then Nothing else Just $
            Whitespace lwsL lws
          , if T.null ill then Nothing else Just $
            Illegal illL ill
          , case afterIll of
            T.Empty -> Nothing
            hash T.:< _
              | classify hash == Hash -> Just $ Comment commentL afterIll
              | otherwise -> internalError "did not consume until comment or eol after start of multi-line string"
          ]
     in (Quote l (MlQuote delim), Left extraTokens, TqLex delim)
  -- error recovery
  Ill ->
    let (ill, r, rest) = _takeClasses (==Ill) (l, c, cs)
     in (Illegal l ill, Right (r, rest), StdLex)

-- > ┌─────┐
-- > │ num │◀──────────────────────────────┐
-- > └─────┘                               │
-- >    │                                  │
-- >    │                                  │
-- >    │──▶ [+-] ────────▶ Sign ─────────▶│
-- >    │                                  │
-- >    │                                  │
-- >    │──▶ 0[boxBOX] ───▶ Radix ────────▶│
-- >    │                                  │
-- >    │                                  │
-- >    │──▶ [:digit:]+ ──▶ Digits ───────▶│
-- >    │                                  │
-- >    │                                  │
-- >    │──▶ [.] ─────────▶ Punctuation ──▶│
-- >    │                                  │
-- >    │                                  │
-- >    │──▶ [:power:] ───▶ Power ────────▶│
-- >    │
-- >    │
-- >    │                               ┌─────┐
-- >    │──▶(?![:digit::power:.+-]) ───▶│ std │
-- >                                    └─────┘

numLex :: Radix -> Pos -> (Char, Text) -> LexerStep
numLex radix l (c, cs) = if
  -- sign
  | Just sign <- parseSign c ->
     (Sign l sign, Right (incCol l, cs), NumLex radix)
  -- radix
  | c == '0'
  , c' T.:< rest <- cs
  , Just newRadix <- parseRadix c' ->
    let r = incCol . incCol $ l
     in (Radix l newRadix, Right (r, rest), NumLex newRadix)
  -- digits
  | isDigitIn radix c ->
    let (digits, r, rest) = _spanBy (isDigitIn radix) (incCol l, cs)
        (i, len) = unwrapOrPanic_ $ (parseDigitsIn radix) (c : T.unpack digits)
     in (Digits l i len, Right (r, rest), NumLex radix)
  -- "decimal" point
  | c == '.' ->
    (Punctuation l Dot, Right (incCol l, cs), NumLex radix)
  -- "times <radix> to the"
  | toLower c == powerLetter radix ->
     (Power l, Right (incCol l, cs), NumLex Base10)
  -- end of number parts, revert to standard
  | otherwise -> stdLex l (c, cs)

powerLetter :: Radix -> Char
powerLetter Base2 = 'p'
powerLetter Base8 = 'p'
powerLetter Base10 = 'e'
powerLetter Base16 = 'p'

-- > ┌───────┐
-- > │ sqstr │◀───────────────────────────────┐
-- > └───────┘                                │
-- >     │                                    │
-- >     │                                    │
-- >     │                                    │
-- >     │──▶ [^'] ──▶ [^']* ──▶ Unescaped ──▶│
-- >     │                                    │
-- >     │                                    │
-- >     │──▶ '' ──────────────▶ Escape ─────▶│
-- >     │
-- >     │
-- >     │                                 ┌─────┐
-- >     │──▶ '(?!') ────▶ CloseStr ──────▶│ std │
-- >                                       └─────┘

sqLex :: Pos -> (Char, Text) -> LexerStep
sqLex l (c, cs) = case classify c of
  SQuote -> case cs of
  -- escaped sequence
    c' T.:< rest | classify c' == SQuote ->
      (StrEscape l "\'", Right (incCol $ incCol l, rest), SqLex)
  -- end of string
    _ -> (Quote l SqlQuote, Right (incCol l, cs), StdLex)
  -- standard string part
  _ ->
    let (str, r, rest) = _takeClasses (/=SQuote) (l, c, cs)
     in (StdStr l str, Right (r, rest), SqLex)

-- |
-- > ┌───────┐
-- > │ dqstr │◀────────────────────────────────────────────────┐
-- > └───────┘                                                 │
-- >     │                                                     │
-- >     │                                                     │
-- >     │                                                     │
-- >     │──▶ [^"`\\] ──▶ [^"`\\]* ─────────────▶ Unescaped ──▶│
-- >     │                                                     │
-- >     │                                                     │
-- >     │──▶ \\ ──┐──▶ [0abefnrt'"`\\&] ───┐                  │
-- >     │         │                        │                  │
-- >     │         │──▶ x[:hex:]{2}      ───│──▶  Escape ─────▶│
-- >     │         │                        │
-- >     │         │──▶ u[:hex:]+        ───┘
-- >     │
-- >     │
-- >     │                                                  ┌─────┐
-- >     │──▶ ["`] ─────────────────────▶ CloseStr ────────▶│ std │
-- >                                                        └─────┘

dqLex :: Pos -> (Char, Text) -> LexerStep
dqLex l (c, cs) = case classify c of
  DQuote -> (Quote l DblQuote, Right (incCol l, cs), StdLex)
  BtQuote -> (Quote l Backtick, Right (incCol l, cs), StdLex)
  Punct Backslash -> case cs of
    c' T.:< cs'
    -- numeric escapes
    -- NOTE I've only allowed lowercase \x and \u sequences, with the idea that uppercase escape chars are reserved
      | c' == 'x' -> escapeX l c c' cs'
      | c' == 'u' -> escapeU l c c' cs'
    -- NOTE perhaps I'll allow control code escapes? like Haskell "\^C"
    -- NOTE ascii control code mnemonics would be nice, possibly
    -- single-letter escape sequence (mostly C escapes)
      | Just str <- lookup c' stdEscapes ->
        (StrEscape l str, Right (incCol $ incCol l, cs'), DqLex)
    -- unrecognized escape
      | otherwise ->
        let str = T.pack [c, c']
            r = advInLine l str
         in (Illegal l str, Right (r, cs'), DqLex)
    -- unexpected end of string
    T.Empty -> (Illegal l (T.singleton c), Left [], StdLex)
  _ ->
    let (str, r, rest) = _takeClasses (`notElem` [DQuote, BtQuote, Punct Backslash]) (l, c, cs)
     in (StdStr l str, Right (r, rest), DqLex)
--- helpers for dqStr ---
escapeX ::
      Pos -- ^ left of the backslash
  -> Char -- ^ the backslash
  -> Char -- ^ the 'x'
  -> Text -- ^ the rest of the line
  -> LexerStep
escapeX l c c' cs' = case cs' of
  (c1 T.:< c2 T.:< rest) ->
    let tok = case parseHex [c1, c2] of
                Just n -> StrEscape l [chr $ fromInteger n]
                Nothing -> Illegal l str
        str = T.pack [c, c', c1, c2]
        r = advInLine l str
    in (tok, Right (r, rest), DqLex)
  (c1 T.:< T.Empty) ->
    let str = T.pack [c, c', c1]
      in (Illegal l str, Left [], StdLex)
  T.Empty ->
    let str = T.pack [c, c']
      in (Illegal l str, Left [], StdLex)
escapeU ::
      Pos -- ^ left of the backslash
  -> Char -- ^ the backslash
  -> Char -- ^ the 'u'
  -> Text -- ^ the rest of the line
  -> LexerStep
escapeU l c c' cs' =
  let hexL = advInLine l (T.pack [c, c'])
      (str, r, rest) = _spanBy isHexDigit (hexL, cs')
      tok = case parseHex (T.unpack str) of
              Just n | n <= 0x10FFFF ->
                StrEscape l [chr $ fromIntegral n]
              _ -> Illegal l str
    in (tok, Right (r, rest), DqLex)

-- |
-- >   ┌───────┐
-- >   │ tqstr │◀─────────────────────────────────┐
-- >   └───────┘                                  │
-- >       │                                      │
-- >       │                                      │
-- >       │──▶ [\t ] ──▶ [\t +] ──▶ Lws ────────▶│
-- >       │                                      │
-- >       ▼                                      │
-- >                                              │
-- > is delimiter? ─── no ──▶ .+ ──▶ Unescaped ──▶│
-- >
-- >       │
-- >                                           ┌─────┐
-- >      yes ───────────▶ CloseStr ──────────▶│ std │
-- >                                           └─────┘
-- >
-- > the delimiter will match the /"{3,}[A-Za-z]*/ found when first entering tqstr

tqLex :: Text -> Pos -> (Char, Text) -> LexerStep
tqLex delim l (c, cs) = case classify c of
  Lws ->
    let (lws, r, rest) = _takeClasses (==Lws) (l, c, cs)
     in (Whitespace l lws, Right (r, rest), TqLex delim)
  _ | str <- c T.:< cs -> case T.stripPrefix delim str of
    Nothing -> (StdStr l str, Left [], TqLex delim)
    Just rest ->
      let r = advInLine l delim
       in (Quote l (MlQuote delim), Right (r, rest), StdLex)

data Classify
  = Id0
  | Num
  | Sgn
  -- punctuation
  | Punct PunctuationType
  | SQuote
  | DQuote
  | BtQuote
  -- whitespace
  | Hash
  | Lws
  -- illegal tokens
  | Ill
  -- TODO rn I'm allowing all 'Ill' chars in strings and comments
  -- some codepoints are illegal everywhere, but some that are fine in strings/comments are illegal in standard mode
  deriving (Eq)

------ Terminals/Alphabet ------

classify :: Char -> Classify
-- NOTE I could actually build a lookup table for 0-127 at compiletime, then handle higher codepoints with a fallback
-- the fallback could probably use the `interval` package to be fairly speedy
classify c
  | '\x00' <= c && c <= '\x1F' = if c == '\t' then Lws else Ill -- TODO I haven't checked that newline and carriage return weren't wrongly passed
  | c == ' ' = Lws
  | c == '!' = Id0
  | c == '\"' = DQuote
  | c == '#' = Hash
  | c == '$' = Id0
  | c == '%' = Id0
  | c == '&' = Id0
  | c == '\'' = SQuote
  | c == '(' = Punct (Open Round)
  | c == ')' = Punct (Close Round)
  | c == '*' = Id0
  | c == '+' = Sgn
  | c == ',' = Punct Comma
  | c == '-' = Sgn
  | c == '.' = Punct Dot
  | c == '/' = Id0
  | '0' <= c && c <= '9' = Num
  | c == ':' = Punct Colon
  | c == ';' = Punct Semicolon
  | c == '<' = Id0
  | c == '=' = Id0
  | c == '>' = Id0
  | c == '?' = Id0
  | c == '@' = Id0
  | 'A' <= c && c <= 'Z' = Id0
  | c == '[' = Punct (Open Square)
  | c == '\\' = Punct Backslash
  | c == ']' = Punct (Close Square)
  | c == '^' = Id0
  | c == '_' = Id0
  | c == '`' = BtQuote
  | 'a' <= c && c <= 'z' = Id0
  | c == '{' = Punct (Open Curly)
  | c == '|' = Id0
  | c == '}' = Punct (Close Curly)
  | c == '~' = Id0
  | otherwise = Ill -- TODO this completely disallows all non-ascii outside strings and comments

stdEscapes :: [(Char, [Char])]
stdEscapes =
  [ ('0', "\0")
  , ('a', "\a")
  , ('b', "\b")
  , ('e', "\ESC")
  , ('f', "\f")
  , ('n', "\n")
  , ('r', "\r")
  , ('t', "\t")
  , ('v', "\v")
  , ('\'', "\'")
  , ('\"', "\"")
  , ('`', "`")
  , ('\\', "\\")
  , ('&', "")
  ]

------ Helpers ------

_takeClasses :: (Classify -> Bool) -> (Pos, Char, Text) -> (Text, Pos, Text)
_takeClasses f (l, c, cs) = (ok, r, rest)
  where
  ok = c T.:< more
  r = advInLine l ok
  (more, rest) = T.span (f . classify) cs

_spanClasses :: (Classify -> Bool) -> (Pos, Text) -> (Text, Pos, Text)
_spanClasses f (l, cs) = (ok, r, rest)
  where
  (ok, rest) = T.span (f . classify) cs
  r = advInLine l ok

_spanBy :: (Char -> Bool) -> (Pos, Text) -> (Text, Pos, Text)
_spanBy f (l, cs) = (ok, r, rest)
  where
  (ok, rest) = T.span f cs
  r = advInLine l ok

parseRadix :: Char -> Maybe Radix
parseRadix c = case toLower c of
  'b' -> Just Base2
  'o' -> Just Base8
  'x' -> Just Base16
  _ -> Nothing

parseSign :: Char -> Maybe Sign
parseSign '+' = Just Positive
parseSign '-' = Just Negative
parseSign _ = Nothing

isDigitIn :: Radix -> Char -> Bool
isDigitIn _ '_' = True
isDigitIn Base2 c = '0' == c || c == '1'
isDigitIn Base8 c = isOctDigit c
isDigitIn Base10 c = isDigit c
isDigitIn Base16 c = isHexDigit c

parseDigitsIn :: Radix -> String -> Maybe (Integer, Int)
parseDigitsIn _ "" = Nothing
parseDigitsIn radix str0 = case radix of
  Base2 -> parseWith 2
  Base8 -> parseWith 8
  Base10 -> parseWith 10
  Base16 -> parseWith 16
  where
  allDigits = "0123456789abcdef"
  parseWith :: Int -> Maybe (Integer, Int)
  parseWith base =
    let loop :: (Integer, Int) -> String -> Maybe (Integer, Int)
        loop acc "" = Just acc
        loop acc ('_' : cs) = loop acc cs
        loop (num, len) (c : cs) = do
          i <- T.findIndex (toLower c ==) digits
          let num' = num * (fromIntegral base) + (fromIntegral i)
          loop (num', len + 1) cs
        digits = T.take base allDigits
     in loop (0, 0) str0

parseHex :: String -> Maybe Integer
parseHex "" = Nothing
parseHex str0 = loop 0 str0
  where
  loop :: Integer -> String -> Maybe Integer
  loop acc "" = Just acc
  loop acc (c : cs)
    | '0' <= c && c <= '9' = loop (acc * 16 + (fromIntegral $ ord c - ord '0')) cs
    | 'a' <= c && c <= 'f' = loop (acc * 16 + (fromIntegral $ ord c - ord 'a' + 10)) cs
    | 'A' <= c && c <= 'F' = loop (acc * 16 + (fromIntegral $ ord c - ord 'A' + 10)) cs
    | otherwise = Nothing

------ Recursion Relation ------

lex :: LexMode -> [Line] -> [Token Span]
lex mode0 = runLinear . go mode0
  where
  go :: LexMode -> [Line] -> Linear (Token Span) LexMode
  go mode (l:ls) = lexLineFrom mode l >>= \newMode -> go newMode ls
  go mode [] = Done mode

lexLineFrom :: LexMode -> Line -> Linear (Token Span) LexMode
lexLineFrom mode line = case (line.content, line.eol) of
  (c T.:< cs, _) -> loopLexer line $ lex1 line.loc.start (c, cs)
  (T.Empty, Eof) -> Done recoverMode
  (T.Empty, ty) ->
    let tok = Eol (unwrapOrPanic_ $ mkSpan line.loc.end (incLine line.loc.end)) ty
     in tok `More` Done recoverMode
  where
  lex1 = case mode of
    StdLex -> stdLex
    NumLex radix -> numLex radix
    SqLex -> sqLex
    DqLex -> dqLex
    TqLex delim -> tqLex delim
  -- | In case we get to the end of line unexpectedly, we need to reset the mode to standard.
  recoverMode = case mode of
    StdLex -> StdLex
    NumLex _ -> StdLex
    SqLex -> StdLex
    DqLex -> StdLex
    TqLex delim -> TqLex delim

-- | The results from lexing a single token from within a line.
-- The algorithm is explained more in 'loopLexer'.
type LexerStep = (Token Pos, Either [Token Pos] (Pos, Text), LexMode)

-- | The lexer algorithm is a loop.
-- We lex a single token with one of 'stdLex, sqLex, dqLex, tqLex'.
-- This token comes with some state data.
-- We emit the token (wrapped into a proper 'Token Span').
-- Then, we inspect the state data to determine the next iteration of the loop (or its termination).
loopLexer :: Line -> LexerStep -> Linear (Token Span) LexMode
loopLexer line (tok, rest, newMode) = oneToken `More` moreTokens
  where
  r = either (const line.loc.end) fst rest
  oneToken = tok <&> \l -> unwrapOrPanic_ (mkSpan l r)
  moreTokens = case rest of
    -- There may be more text left in the line:
    -- update line state and continue to lex in the new mode
    Right (newPos, newContent) -> lexLineFrom newMode Line
      { content = newContent
      , loc = unwrapOrPanic_ $ mkSpan newPos line.loc.end
      , eol = line.eol
      }
    -- The line is explicitly drained:
    -- we'll first emit any extra tokens, then
    -- create an empty line with the eol (and its position) intact,
    -- which enables 'lexFromLine' to emit the eol token if needed
    Left toks -> _extraTokens toks line $
      lexLineFrom newMode Line
        { content = T.empty
        , loc = spanFromPos line.loc.end
        , eol = line.eol
        }

_extraTokens :: [Token Pos] -- extra tokens to emit
  -> Line -- the rest of the line
  -> Linear (Token Span) LexMode -- token stream after the input tokens, dependent on the Line argument
  -> Linear (Token Span) LexMode
_extraTokens [] _ z = z
_extraTokens [x] line z = tok `More` z
  where tok = x <&> \l -> unwrapOrPanic_ $ mkSpan l line.loc.end
_extraTokens (x:xs@(next:_)) line z = tok `More` _extraTokens xs line z
  where tok = x <&> \l -> unwrapOrPanic_ $ mkSpan l (annotation next)

-------------------------
------ Linear Data ------
-------------------------

-- TODO this is really more of a Streaming.Pure module

data Linear a z
  = More a (Linear a z)
  | Done z

deriving instance Functor (Linear a)

instance Applicative (Linear a) where
  pure = Done
  (More _ xs) <*> ys = xs <*> ys
  (Done f) <*> ys = f <$> ys
instance Monad (Linear a) where
  (More x xs) >>= k = More x (xs >>= k)
  (Done z) >>= k = k z

runLinear :: Linear a z -> [a]
runLinear (More x xs) = x : runLinear xs
runLinear (Done _) = []
