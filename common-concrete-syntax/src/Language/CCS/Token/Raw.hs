{-# LANGUAGE MultiWayIf #-}

module Language.CCS.Token.Raw
  ( CCS(..)
  , Token(..)
  , PunctToken(..)
  , QuoteToken(..)
  , StringPart(..)
  , tokenize
  ) where

import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Language.CCS.Location (Span(..), Off(..))
import Language.Nanopass (deflang)
import Streaming.Prelude (Stream, Of(..))

import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
((CCS loc)

  (Token
    (Symbol loc Text)
    (IntLit loc Integer)
    (Punctuation loc PunctToken)
    (Quote loc QuoteToken)
    (StdStr loc Text)
    (StrEscape loc (* Char))
    (Newline loc)
    (Whitespace loc Text)
    (Comment loc Text)
    (Illegal loc Text)
  )

)
|]
-- TODO the argument for Symbol should enforce the invariants on how Symbols may be spelled
-- TODO FloLit for floating point literals

deriving instance Show a => Show (Token a)

data PunctToken
  = Open Enclose
  | Close Enclose
  | Comma | Dot | Colon | Semicolon
  | Backslash
  deriving (Eq, Show)
data Enclose = Round | Square | Curly
  deriving (Eq, Show)

data QuoteToken
  = DblQuote
  | Backtick
  -- TODO multi-line string quote
  deriving (Eq, Show)

data StringPart
  = PlainStr Text
  | Escape Text
  deriving (Show)

toCodepoint :: Integer -> Maybe Char
toCodepoint i = if 0 <= i && i <= 0x10FFFF then Just (chr $ fromIntegral i) else Nothing

------------------------
------ Tokenizing ------
------------------------


--                                                      :id0: === !$%&*/<=>?^_|        
--                                                                                     
--                                                       :id: === :id0:0-9+-           
--                                                                                     
--                                        ┌─────┐                                      
-- ┌─────────────────────────────────────▶│ std │◀────────────────────────────────────┐
-- │                                      └─────┘                                     │
-- │                                         │                                        │
-- │                                         │                                        │
-- │──────────── Comment ◀──────── [#][^\n]* │ [:id0:]───┐                            │
-- │                                         │           │──▶[:id:]* ─▶ Identifier ───│
-- │                                         │           │                            │
-- │── Linear Whitespace ◀─────────── [\t ]+ │ [+-]──────┘                            │
-- │                                         │   │                                    │
-- │                                         │   ▼                      ┌─────┐       │
-- │──────────── Newline ◀───────────── [\n] │ [0-9] ──────────────────▶│ num │───────│
-- │                                         │                          └─────┘       │
-- │                                         │                                        │
-- │──────── Punctuation ◀── [()[\]{},.:;\\] │ [']([^'\n]|'')*['] ─────▶ SqlStr ──────│
-- │                                         │                                        │
-- │           ┌───────┐                     │                          ┌─────┐       │
-- │────────── │ mlstr │ ◀────────── ["]{3,} │ ["`] ───────────────────▶│ str │───────│
--             └───────┘                     │                          └─────┘        
-- 
-- 
-- Edit/view: https://cascii.app/725d6

tokenize :: (Monad m) => Text -> m [Token Span]
tokenize input =
  T.unpack input
  & S.each
  & charSpans Off { line = 1, col = 1 } -- TODO allow for alternate start locations
  & stdMode
  & S.toList
  & fmap (\(toks :> _) -> toks)

data StdClassify
  = Id
  | Num
  | Sign
  -- punctuation
  | Punct PunctToken
  | QuoDbl | QuoTick
  -- whitespace
  | Hash
  | Lws
  | Nl
  -- illegal tokens
  | Ill
  deriving (Eq)

stdMode :: (Monad m) => Stream (Of (Char, Span)) m r -> TokenStream m r
stdMode s = S.effect $ S.next s <&> \case
  Left done -> pure done
  -- Here's how this works.
  -- We look at the next character, and dispatch based on `classify`.
  -- In each branch, we have
  --   the character `c`,
  --   it's location `l`,
  --   and the rest of the strweam `s'`.
  -- Each branch must produce a token stream, in one of two ways:
  --   - We just yield a token, then tail-call another mode `stdMode, strMode, mlStrMode`.
  --   - We need to consume more off the character stream.
  --     First, we're gonna need to wrap the `CharStream` manipulation in `effect`.
  --     Finally, I like to create the `TokenStream` with a `pure $ do ...`.
  --     Everything else in-between is just whatever it takes to analyze an transform the input stream.
  Right ((c, l), s') -> case classify c of
    Id -> S.effect $ do
      ((cs, r) :> s'') <- takeText l.end continuesId s'
      let tok = Symbol (Span l.start r) (T.cons c cs)
      pure $ do
        S.yield tok
        stdMode s''
    Num -> S.effect $ do
      unimplemented "numbers"
    Sign -> S.effect $ do
      (isNum, s'') <- test1 s' ((Num==) . classify)
      if isNum
      then do -- is a number
        unimplemented "plus and minus"
      else do -- just a symbol
        ((cs, r) :> s''') <- takeText l.end continuesId s''
        let tok = Symbol (Span l.start r) (T.cons c cs)
        pure $ do
          S.yield tok
          stdMode s'''
    Punct p -> do
      S.yield $ Punctuation l p
      stdMode s'
    QuoDbl -> S.effect $ do
      (isTriple, s'') <- test2 s' $ \a b -> classify a == QuoDbl && classify b == QuoDbl
      case isTriple of
        False -> pure $ do
          S.yield $ Quote l DblQuote
          strMode s''
        True -> do
          ((cs, r) :> s''') <- takeText l.end ((QuoDbl ==) . classify) s''
          let quotes = T.cons c cs
          -- FIXME look for identifier
          -- FIXME skip (error on non-lws) to end of line
          -- FIXME consume end of line
          -- FIXME goto mlstrMode
          pure $ do
            unimplemented "start ml string"
            mlstrMode s'''
    QuoTick -> do
      S.yield $ Quote l Backtick
      strMode s'
    Hash -> S.effect $ do
      ((content, r) :> s'') <- takeText l.end ((Nl /=) . classify) s'
      let tok = Comment (Span l.start r) (T.cons c content)
      pure $ do
        S.yield tok
        stdMode s''
    Lws -> S.effect $ do
      ((cs, r) :> s'') <- takeText l.end ((Lws ==) . classify) s'
      let tok = Whitespace (Span l.start r) (T.cons c cs)
      pure $ do
        S.yield tok
        stdMode s''
    Nl -> do
      S.yield $ Newline l
      stdMode s'
    Ill -> S.effect $ do
      ((content, r) :> s'') <- takeText l.end ((Ill ==) . classify) s'
      let tok = Illegal (Span l.start r) (T.cons c content)
      pure $ do
        S.yield tok
        stdMode s''
  where
  continuesId :: Char -> Bool
  continuesId c = case classify c of
    Id -> True
    Sign -> True
    Num -> True
    _ -> False
  classify :: Char -> StdClassify
  classify c
    | '\x00' <= c && c <= '\x1F' =
      -- NOTE I've not done any special processing for `\r`
      if | c == '\t' -> Lws
         | c == '\n' -> Nl
         | otherwise -> Ill
    | c == ' ' = Lws
    | c == '!' = Id
    | c == '\"' = QuoDbl
    | c == '#' = Hash
    | c == '$' = Id
    | c == '%' = Id
    | c == '&' = Id
    | c == '\'' = unimplemented "single tick" -- TODO
    | c == '(' = Punct (Open Round)
    | c == ')' = Punct (Close Round)
    | c == '*' = Id
    | c == '+' = Sign
    | c == ',' = Punct Comma
    | c == '-' = Sign
    | c == '.' = Punct Dot
    | c == '/' = Id
    | '0' <= c && c <= '9' = Num
    | c == ':' = Punct Colon
    | c == ';' = Punct Semicolon
    | c == '<' = Id
    | c == '=' = Id
    | c == '>' = Id
    | c == '?' = Id
    | c == '@' = Id
    | 'A' <= c && c <= 'Z' = Id
    | c == '[' = Punct (Open Square)
    | c == '\\' = Punct Backslash
    | c == ']' = Punct (Close Square)
    | c == '^' = Id
    | c == '_' = Id
    | c == '`' = QuoTick
    | 'a' <= c && c <= 'z' = Id
    | c == '{' = Punct (Open Curly)
    | c == '|' = Id
    | c == '}' = Punct (Close Curly)
    | c == '~' = Id
    | otherwise = Ill -- NOTE this completely disallows all non-ascii outside strings and comments

data StrClassify
  = Std
  | Esc
  | QuoDblStr | QuoTickStr
  | NlStr
  | IllStr
  deriving (Eq)

strMode :: (Monad m) => CharStream m r -> TokenStream m r
strMode s = S.effect $ S.next s <&> \case
  Left done -> pure done
  Right ((c, l), s') -> case classify c of
    Std -> S.effect $ do
      ((cs, r) :> s'') <- takeText l.end ((Std ==) . classify) s'
      let tok = StdStr (Span l.start r) (T.cons c cs)
      pure $ do
        S.yield tok
        strMode s''
    Esc -> S.effect $ S.next s' <&> \case
      Left done -> do
          S.yield $ Illegal l (T.singleton c)
          pure done
      Right ((c', r), s'') -> case lookup c' stdEscapes of
        Just escValue -> do
          let tok = StrEscape (Span l.start r.end) escValue
          S.yield tok
          strMode s''
        Nothing
          | c' == 'x' -> S.effect $ do
            (cs, r', rest) <- take2 l.start s''
            let badTok = Illegal (Span l.start r') (T.cons c $ T.cons c' cs)
                tok = case parseHex cs >>= toCodepoint of
                  Just code -> StrEscape (Span l.start r') [code]
                  Nothing -> badTok
            case rest of
              Left done -> pure $ do
                S.yield badTok
                pure done
              Right s''' -> pure $ do
                S.yield tok
                strMode s'''
          |  c' == 'u'
          || c' == 'U' -> S.effect $ do
            ((cs, r') :> s''') <- takeText r.end isHexDigit s''
            let tok = case parseHex cs >>= toCodepoint of
                  Just code -> StrEscape (Span l.start r') [code]
                  Nothing -> Illegal (Span l.start r') (T.cons c $ T.cons c' cs)
            pure $ do
              S.yield tok
              strMode s'''
          | otherwise -> do
            S.yield $ Illegal (Span l.start r.end) (T.pack [c, c'])
            strMode s''
    QuoDblStr -> do
      S.yield $ Quote l DblQuote
      stdMode s'
    QuoTickStr -> do
      S.yield $ Quote l Backtick
      stdMode s'
    NlStr -> do
      let s'' = S.cons (c, l) s'
      stdMode s''
    IllStr -> S.effect $ do
      ((cs, r) :> s'') <- takeText l.end ((IllStr ==) . classify) s'
      let tok = Illegal (Span l.start r) (T.cons c cs)
      pure $ do
        S.yield tok
        stdMode s''
  where
  classify c
    | c ==  '\n' = NlStr
    | c <= '\x1F' = IllStr
    | c == '\"' = QuoDblStr
    | c == '`' = QuoTickStr
    | c == '\'' = Std
    | c == '\\' = Esc
    | ' ' <= c && c <= '}' = Std
    | '\x80' <= c = unimplemented "unicode string contents"
    | otherwise = IllStr -- TODO this shouldn't be possible
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


mlstrMode :: Stream (Of (Char, Span)) m r -> TokenStream m r
mlstrMode = unimplemented "multi-line string mode"

-- I've decided to use the streaming library instead of just chunking off bits from a `Text`.
-- The streaming combinators make it much easier to stomach doing things char-by-char.
-- Basically, I trust the performance of the streaming library more than my own ability to write fusible list consumers.

-- | Adds location information into a character stream.
-- It's easier IMO to just have the location info for every character already there, rather than try to compute it as part of the lexer's state.
charSpans :: Monad m => Off -> Stream (Of Char) m r -> Stream (Of (Char, Span)) m r
charSpans off0 = S.scanned update (Span off0 off0) id
  where
  update (Span _ off) c = Span off (off `advChar` c)
  advChar :: Off -> Char -> Off
  advChar off '\n' = off{line = off.line+1, col = 1}
  -- NOTE should I care about any other newline sequences?
  advChar off _ = off{col = off.col+1}

takeText :: (Monad m)
  => Off -- ^ initial position (in case I can't get it, ie from an empty input stream)
  -> (Char -> Bool) -- ^ accumulate text while this is true
  -> CharStream m r -- ^ input stream
  -> m (Of (Text, Off) (CharStream m r)) -- ^ accumulated text and final position
takeText l p stream = S.fold
  (\(txt, _) (c', r') -> (T.snoc txt c', r'.end))
  ("", l)
  id
  (S.span (p . fst) stream)

test1 :: (Monad m)
  => CharStream m r
  -> (Char -> Bool)
  -> m (Bool, CharStream m r)
test1 s p = S.next s >>= \case
  Left done -> pure (False, pure done)
  Right (a, s') -> do
    let s'' = S.cons a s'
    pure (p (fst a), s'')

test2 :: (Monad m)
  => CharStream m r
  -> (Char -> Char -> Bool)
  -> m (Bool, CharStream m r)
test2 s p = S.next s >>= \case
  Left done -> pure (False, pure done)
  Right (a, s') -> S.next s' >>= \case
    Left done -> pure (False, S.cons a (pure done))
    Right (b, s'') -> do
      let s''' = S.cons a (S.cons b s'')
      pure (p (fst a) (fst b), s''')

take2 :: (Monad m)
  => Off
  -> CharStream m r
  -> m (Text, Off, Either r (CharStream m r))
take2 off0 s = S.next s >>= \case
  Left done -> pure ("", off0, Left done)
  Right (a, s') -> S.next s' >>= \case
    Left done -> pure (T.singleton $ fst a, (snd a).end, Left done)
    Right (b, s'') -> pure (T.cons (fst a) (T.singleton $ fst b), (snd b).end, Right s'')

type CharStream m r = Stream (Of (Char, Span)) m r
type TokenStream m r = Stream (Of (Token Span)) m r

unimplemented :: String -> a
unimplemented msg = error $ "UNIMPLEMENTED: " ++ msg

isHexDigit :: Char -> Bool
isHexDigit c
  =  '0' <= c && c <= '9'
  || 'a' <= c && c <= 'f'
  || 'A' <= c && c <= 'F'

parseHex :: Text -> Maybe Integer
parseHex "" = Nothing
parseHex str0 = loop 0 str0
  where
  loop :: Integer -> Text -> Maybe Integer
  loop acc T.Empty = Just acc
  loop acc (c T.:< cs)
    | '0' <= c && c <= '9' = loop (acc * 16 + (fromIntegral $ ord c - ord '0')) cs
    | 'a' <= c && c <= 'f' = loop (acc * 16 + (fromIntegral $ ord c - ord 'a' + 10)) cs
    | 'A' <= c && c <= 'F' = loop (acc * 16 + (fromIntegral $ ord c - ord 'A' + 10)) cs
    | otherwise = Nothing

