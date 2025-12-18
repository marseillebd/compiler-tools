module Language.CCS.Lexer.Pipeline
  ( pipeline
  , pipelineFrom
  , decode
  , lines
  , linesFrom
  ) where

import Prelude hiding (lex, lines, exp)

import Control.Applicative ((<|>))
import Data.Char (chr, ord, isOctDigit, isDigit, isHexDigit, toLower)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Language.CCS.Error (placeholder, internalError, unwrapOrPanic_)
import Language.CCS.Lexer.Morpheme (PunctuationType(..), BracketType(..))
import Language.CCS.Lexer.Morpheme (QuoteType(..), EolType(..))
import Language.CCS.Lexer.Morpheme (Sign(..), Radix(..))
import Language.CCS.Lexer.Morpheme (Token(..), StrToken(..))
import Language.Location (mkSpan, spanFromPos, Pos, startPos, incLine, incCol)
import Language.Text (SrcText)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as Codec
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Codec
import qualified Language.Text as Src

pipeline :: LBS.ByteString -> [Token]
pipeline = pipelineFrom startPos

pipelineFrom :: Pos -> LBS.ByteString -> [Token]
pipelineFrom pos0 bytes = bytes
  & decode
  & linesFrom pos0
  & lexLines

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

data Line a = Line
  { line :: a
  , eol :: EolType
  }

lines :: LT.Text -> [Line SrcText]
lines = linesFrom startPos

linesFrom :: Pos -> LT.Text -> [Line SrcText]
linesFrom _ LT.Empty = [] -- NOTE this means a zero-byte file will have an empty list of lines; anything else will have at least one line (possibly blank ofc)
linesFrom l str = Line
  { line = Src.fromPos l (LT.toStrict lazyContent)
  , eol
  } : maybe [] (linesFrom nextLine) rest
  where
  (lazyContent, eol, rest) = case LT.break (\c -> c == '\n' || c == '\r') str of
    (pre,                   LT.Empty) -> (pre, Eof,  Nothing)
    (pre,            '\n' LT.:< post) -> (pre, LF,   Just post)
    (pre, '\r' LT.:< '\n' LT.:< post) -> (pre, CRLF, Just post)
    (pre, '\r' LT.:<            post) -> (pre, CR,   Just post)
    _ -> internalError "expected an end of line sequence after preaking on `\\n|\\r`"
  nextLine = incLine l

----------------------------
------ Coverage Lexer ------
----------------------------

-- We use @lex*@ to indicate functions that produce tokens based on 'Src.consume'.
-- Thus, they are not composable, but can be "tail-called" from another lex-fuction.
-- Meanwhile @take*@ functions produce output based only on what it consumes.
-- Thus, they _are_ composable, and can be used freely in other take- or lex-functions.

------ Main ------

lexLines :: [Line SrcText] -> [Token]
lexLines = loop StdLex
  where
  loop :: LexMode -> [Line SrcText] -> [Token]
  loop mode (l:ls) = toks ++ (end : loop mode' ls)
    where
    (toks, mode') = lexLine mode l.line
    end = case l.eol of
      Eof -> Eol (spanFromPos l.line.span.end) l.eol
      _ -> Eol (unwrapOrPanic_ $ mkSpan l.line.span.end (incLine l.line.span.end)) l.eol
  loop _ [] = []

data LexMode
  = StdLex
  | TqLex Text

lexLine :: LexMode -> SrcText -> ([Token], LexMode)
lexLine StdLex src0 = loop [] src0
  where
  loop revacc src = case lexStd src of
    Just (tok@(MlDelim delim), rest) -> drain delim.text (tok : revacc) rest
    Just (tok, rest) -> loop (tok : revacc) rest
    Nothing -> (reverse revacc, StdLex)
  drain delim revacc src = case lexAfterMlDelim src of
    Just (tok, rest) -> drain delim (tok : revacc) rest
    Nothing -> (reverse revacc, TqLex delim)
lexLine (TqLex delim) src0 = case Src.runParse src0 lexDelim of
  Just (_, revacc, rest) -> drain revacc rest
  Nothing -> ([MlContent src0], TqLex delim)
  where
  lexDelim = do
    ws <- Src.asSrc $ Src.takeWhile (`T.elem` " \t")
    delim' <- Src.asSrc $ Src.takePrefix delim
    pure $ MlClose delim'.span : (if Src.null ws then [] else [Whitespace ws])
  drain revacc src = case lexAfterMlDelim src of
    Just (tok, rest) -> drain (tok : revacc) rest
    Nothing -> (reverse revacc, StdLex)

lexTq :: Text -> SrcText -> Maybe (Token, SrcText)
lexTq _ src | Src.null src = Nothing
lexTq delim src = case Src.runParse src parser of
  Just (_, tok, rest) -> Just (tok, rest)
  Nothing -> internalError "coverage token parsing failed somehow"
  where
  parser
     =  do
        _ <- Src.takeWhile (`T.elem` " \t")
        _ <- Src.takePrefix delim
        delim' <- Src.consumed
        pure $ MlDelim delim'
    <|> do
        _ <- Src.takeAll
        MlContent <$> Src.consumed

lexStd :: SrcText -> Maybe (Token, SrcText)
lexStd src | Src.null src = Nothing
lexStd src = case Src.runParse src parser of
  Just (_, tok, rest) -> Just (tok, rest)
  Nothing -> internalError "coverage token parsing failed somehow"
  where
  parser = do
    c <- Src.sat (const True)
    case classify c of
    -- numbers and symbols
      Id0 -> lexSymbol
      Num -> do
        (radix, firstDigit) <- takeRadix (Just c)
        lexNumber Positive radix firstDigit
      Sgn sign -> do
        isNum <- Src.look 1 <&> \case
          next T.:< _ -> isDigitIn Base10 next
          _ -> False
        if isNum
        then do
          (radix, firstDigit) <- takeRadix Nothing
          lexNumber sign radix firstDigit
        else lexSymbol
    -- punctuation
      Punct (Dots _) -> do
        _ <- Src.takeWhile (== '.')
        dots <- Src.consumed
        pure $ Punctuation dots.span (Dots $ T.length dots.text)
      Punct (Colons _) -> do
        _ <- Src.takeWhile (== ':')
        colons <- Src.consumed
        pure $ Punctuation colons.span (Colons $ T.length colons.text)
      Punct ty ->
        Punctuation <$> (Src.consumed <&> (.span)) <*> pure ty
    -- strings
      SQuote -> lexSqString
      DQuote -> do
        isTqStr <- Src.tryN 2 $ \case
          "\"\"" -> Right True
          _ -> Left False
        if not isTqStr
        then lexDqString DblQuote
        else lexTqStr
      BtQuote -> lexDqString Backtick
    -- whitespace
      Lws -> do
        _ <- Src.takeWhile (`T.elem` " \t")
        Whitespace <$> Src.consumed
      Hash -> do
        _ <- Src.takeAll
        Comment <$> Src.consumed
    -- illegal tokens
      Ill -> do
        _ <- Src.takeWhile ((== Ill) . classify)
        Illegal <$> Src.consumed

------ Symbols ------

lexSymbol :: Src.Parse Token
lexSymbol = do
  _ <- Src.takeWhile isId
  Symbol <$> Src.consumed
  where
  isId c = case classify c of
    Id0 -> True
    Num -> True
    Sgn _ -> True
    _ -> False

------ Numbers ------

lexNumber :: Sign -> Radix -> Text -> Src.Parse Token
lexNumber sign radix firstDigit = do
  -- integer part
  intDigits <- takeDigits radix firstDigit
  let intPart = fst $ parseDigitsIn radix (T.unpack intDigits)
  -- fractional part
  hasDecimalPoint <- takeDecimalPoint
  fracDigits <- if hasDecimalPoint
    then Just <$> Src.takeWhile (isDigitIn radix)
    else pure Nothing
  let fracPart = parseDigitsIn radix . T.unpack <$> fracDigits
  -- exponent
  hasExpMarker <- takeExpMarker radix
  exp <- if hasExpMarker
    then do
      expSign <- takeSign
      expDigits <- takeDigits Base10 ""
      let (expMag, _) = parseDigitsIn Base10 (T.unpack expDigits)
          expPart = case expSign of
            Positive -> expMag
            Negative -> negate expMag
      pure $ Just expPart
    else pure Nothing
  loc <- Src.consumed <&> (.span)
  pure $ Number loc
    sign radix intPart
    fracPart exp

takeSign :: Src.Parse Sign
takeSign = Src.tryN 1 $ \case
  "+" -> Right Positive
  "-" -> Right Negative
  _ -> Left Positive

takeRadix :: Maybe Char -> Src.Parse (Radix, Text)
takeRadix Nothing = Src.tryN 2 $ \next2 -> case T.toLower next2 of
  '0' T.:< "b" -> Right (Base2, "")
  '0' T.:< "o" -> Right (Base8, "")
  '0' T.:< "x" -> Right (Base16, "")
  _ -> Left (Base10, "")
takeRadix (Just firstDigit) = Src.tryN 1 $ \next ->
  case (firstDigit, T.toLower next) of
    ('0', "b") -> Right (Base2, "")
    ('0', "o") -> Right (Base8, "")
    ('0', "x") -> Right (Base16, "")
    _ -> Left (Base10, T.singleton firstDigit)

takeDigits :: Radix -> Text -> Src.Parse Text
takeDigits radix firstDigit = (firstDigit <>) <$> Src.takeWhile (isDigitIn radix)

takeDecimalPoint :: Src.Parse Bool
takeDecimalPoint = Src.tryN 1 $ \case
  "." -> Right True
  _ -> Left False

takeExpMarker :: Radix -> Src.Parse Bool
takeExpMarker Base10 = Src.tryN 1 $ \next ->
  if T.toLower next == "e" then Right True else Left False
takeExpMarker _ = Src.tryN 1 $ \next ->
  if T.toLower next == "p" then Right True else Left False

------ Strings ------

--- SQL Strings ---

lexSqString :: Src.Parse Token
lexSqString = do
  parts <- takeSqs
  closeQuote <- Src.tryN 1 $ \case
    "'" -> Right $ Just SqlQuote
    _ -> Left Nothing
  loc <- Src.consumed <&> (.span)
  pure $ Str loc SqlQuote parts closeQuote

takeSqs :: Src.Parse [StrToken]
takeSqs = loop []
  where
  loop revacc = takeSq >>= \case
    Just tok -> loop (tok : revacc)
    Nothing -> pure $ reverse revacc

takeSq :: Src.Parse (Maybe StrToken)
takeSq = Src.branch
  [ (takeSqStd, pure . Just)
  , (takeSqEsc, pure . Just)
  , (pure undefined, pure . const Nothing)
  ]

takeSqStd :: Src.Parse StrToken
takeSqStd = do
  src <- Src.asSrc $ Src.takeWhile1 (/= '\'')
  pure $ StdStr src

takeSqEsc :: Src.Parse StrToken
takeSqEsc = do
  (loc, _) <- Src.withSpan $ do
    _ <- Src.sat (== '\'')
    _ <- Src.sat (== '\'')
    pure ()
  pure $ StrEscape loc '\''

--- Double-quoted Strings ---

lexDqString :: QuoteType -> Src.Parse Token
lexDqString openQuote = do
  parts <- takeDqs
  closeQuote <- takeQuote
  loc <- Src.consumed <&> (.span)
  pure $ Str loc openQuote parts closeQuote

takeQuote :: Src.Parse (Maybe QuoteType)
takeQuote = Src.tryN 1 $ \case
  "\"" -> Right $ Just DblQuote
  "`" -> Right $ Just Backtick
  _ -> Left Nothing

takeDqs :: Src.Parse [StrToken]
takeDqs = loop []
  where
  loop revacc = takeDq >>= \case
    Just tok -> loop (tok : revacc)
    Nothing -> pure $ reverse revacc

takeDq :: Src.Parse (Maybe StrToken)
takeDq = Src.branch
  [ (takeDqStd, pure . Just)
  , (takeDqEsc, pure . Just)
  , (takeDqIllegal, pure . Just)
  , (pure undefined, pure . const Nothing)
  ]

takeDqStd :: Src.Parse StrToken
takeDqStd = do
  src <- Src.asSrc $ Src.takeWhile1 (not . (`T.elem` "\"`\\"))
  pure $ StdStr src

takeDqEsc :: Src.Parse StrToken
takeDqEsc = do
  (loc, tokE) <- Src.withSpan $ stdEsc <|> xEsc <|> uEsc
  pure $ case tokE of
    Right code -> StrEscape loc code
    Left err -> IllStr $ Src.fromPos loc.start err
  where
  stdEsc :: Src.Parse (Either Text Char)
  stdEsc = do
    _ <- Src.sat (== '\\')
    char <- Src.sat (`elem` (fst <$> stdEscapes))
    pure $ Right . unwrapOrPanic_ $ lookup char stdEscapes
  xEsc :: Src.Parse (Either Text Char)
  xEsc = do
    -- backslash + x
    bs <- Src.sat (== '\\')
    x <- Src.sat (== 'x')
    -- two digits
    aE <- (Right <$> Src.sat isHexDigit) <|> (Left <$> Src.take 1)
    bE <- (Right <$> Src.sat isHexDigit) <|> (Left <$> Src.take 1)
    -- parse digits or form error
    pure $ case (aE, bE) of
      (Right a, Right b) ->
        let code = unwrapOrPanic_ $ parseHex [a, b]
         in Right (chr $ fromInteger code)
      _ ->
        let a = either id T.singleton aE
            b = either id T.singleton bE
        in Left $ T.pack [bs, x] <> a <> b
  uEsc :: Src.Parse (Either Text Char)
  uEsc = do
    -- backslash + u
    bs <- Src.sat (== '\\')
    u <- Src.sat (== 'u')
    -- open bracket, hex digits, close bracket
    openM <- (Just <$> Src.sat (== '{')) <|> pure Nothing
    codeE <- (Right <$> Src.takeWhile1 isHexDigit) <|> (Left <$> pure "")
    closeM <- (Just <$> Src.sat (== '}')) <|> pure Nothing
    -- parse digits or form error
    pure $ case (openM, codeE, closeM) of
      (Just _, Right digits, Just _)
        | code <- unwrapOrPanic_ $ parseHex (T.unpack digits)
        , 0 <= code && code <= 0x10FFFF
        , not (0xD000 <= code && code <= 0xDFFF) -- surrogate pairs are invalid codepoints
        -> Right (chr $ fromInteger code)
      _ ->
        let open = maybe "" T.singleton openM
            digits = either id id codeE
            close = maybe "" T.singleton closeM
         in Left $ T.pack [bs, u] <> open <> digits <> close

takeDqIllegal :: Src.Parse StrToken
takeDqIllegal = do
  -- TODO perhaps other characters should be illegal, outisde of backslash (and doublequote/backtick, which end the token)
  (loc, bs) <- Src.withSpan $ T.singleton <$> Src.sat (== '\\')
  pure $ IllStr $ Src.fromPos loc.start bs

--- Multi-line Strings ---

lexTqStr :: Src.Parse Token
lexTqStr = do
  -- we've already checked for three quotes before
  -- now take remaining quotes
  _ <- Src.takeWhile (== '\"')
  _ <- Src.takeWhile (\c -> 'A' <= c && c <= 'Z'
                      || 'a' <= c && c <= 'z')
  MlDelim <$> Src.consumed

lexAfterMlDelim :: SrcText -> Maybe (Token, SrcText)
lexAfterMlDelim src | Src.null src = Nothing
lexAfterMlDelim src = case Src.runParse src parser of
  Just (_, tok, rest) -> Just (tok, rest)
  Nothing -> internalError "coverage token parsing failed somehow"
  where
  parser = do
    c <- Src.sat (const True)
    case classify c of
    -- whitespace
      Lws -> do
        _ <- Src.takeWhile (`T.elem` " \t")
        Whitespace <$> Src.consumed
      Hash -> do
        _ <- Src.takeAll
        Comment <$> Src.consumed
    -- everything else is illegal
      _ -> do
        _ <- Src.takeWhile (not . (`T.elem` " \t#"))
        Illegal <$> Src.consumed

----------------------------
------ DELME ------
----------------------------

------ Terminals/Alphabet ------

data Classify
  = Id0
  | Num
  | Sgn Sign
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
  | c == '+' = Sgn Positive
  | c == ',' = Punct Comma
  | c == '-' = Sgn Negative
  | c == '.' = Punct $ Dots 1
  | c == '/' = Id0
  | '0' <= c && c <= '9' = Num
  | c == ':' = Punct $ Colons 1
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

stdEscapes :: [(Char, Char)]
stdEscapes =
  [ ('0', '\0')
  , ('a', '\a')
  , ('b', '\b')
  , ('e', '\ESC')
  , ('f', '\f')
  , ('n', '\n')
  , ('r', '\r')
  , ('t', '\t')
  , ('v', '\v')
  , ('\'', '\'')
  , ('\"', '\"')
  , ('`', '`')
  , ('\\', '\\')
  ]

------ Helpers ------

isDigitIn :: Radix -> Char -> Bool
isDigitIn _ '_' = True
isDigitIn Base2 c = '0' == c || c == '1'
isDigitIn Base8 c = isOctDigit c
isDigitIn Base10 c = isDigit c
isDigitIn Base16 c = isHexDigit c

parseDigitsIn :: Radix -> String -> (Integer, Int)
parseDigitsIn _ "" = (0, 0)
parseDigitsIn radix str0 = unwrapOrPanic_ $ case radix of
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
