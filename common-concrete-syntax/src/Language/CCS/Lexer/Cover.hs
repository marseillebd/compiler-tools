module Language.CCS.Lexer.Cover
  ( CCS(..)
  , Token(..)
  , StrToken(..)
  , PunctuationType(..)
  , BracketType(..)
  , Sign(..)
  , Radix(..)
  , QuoteType(..)
  , EolType(..)
  , lexLines
  ) where

import Prelude hiding (exp)

import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Data.Char (chr, ord, toLower, isDigit, isOctDigit, isHexDigit)
import Data.Functor ((<&>))
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.CCS.Error (unwrapOrPanic_, internalError)
import Language.CCS.Lexer.Decode (Line(..), EolType(..))
import Language.Location (Span, mkSpan, spanFromPos, incLine)
import Language.Nanopass (deflang)
import Language.Text (SrcText)

import qualified Data.Text as T
import qualified Language.Text as Src

----------------------------
------ Token Language ------
----------------------------

[deflang|
(CCS

  (Token
    (Symbol SrcText)
    (Number Span
      Sign
      Radix
      Integer
      (? (& Integer Int))
      (? Integer)
    )
    (Str Span QuoteType (* StrToken) (? QuoteType))
    (MlDelim SrcText)
    (MlContent SrcText)
    (MlClose Span)
    (Punctuation Span PunctuationType)
    (Eol Span EolType)
    (Whitespace SrcText)
    (Comment SrcText)
    (Illegal SrcText)
  )

  (StrToken
    (StdStr SrcText)
    (StrEscape Span Char)
    (IllStr SrcText)
  )

  (PunctuationType
    (Open BracketType)
    (Close BracketType)
    (Dots Int)
    (Colons Int)
    (Comma)
    (Semicolon)
    (Backslash)
  )

)
|]
-- TODO the argument for Symbol should enforce the invariants on how Symbols may be spelled

deriving instance Show Token
deriving instance Show StrToken
deriving instance Show PunctuationType
deriving instance Eq PunctuationType

instance HasField "span" Token Span where
  getField (Symbol a) = a.span
  getField (Number a _ _ _ _ _) = a
  getField (Str a _ _ _) = a
  getField (MlDelim a) = a.span
  getField (MlContent a) = a.span
  getField (MlClose a) = a
  getField (Punctuation a _) = a
  getField (Eol a _) = a
  getField (Whitespace a) = a.span
  getField (Comment a) = a.span
  getField (Illegal a) = a.span

data BracketType = Round | Square | Curly
  deriving (Eq, Show)

data Sign = Positive | Negative
  deriving (Eq, Show)

data Radix = Base2 | Base8 | Base10 | Base16
  deriving (Eq, Show)

instance HasField "base" Radix Integer where
  getField = \case
    Base2 -> 2
    Base8 -> 8
    Base10 -> 10
    Base16 -> 16

data QuoteType
  = SqlQuote
  | DblQuote
  | Backtick
  | MlQuote Text
  deriving (Eq, Show)

----------------------------
------ Coverage Lexer ------
----------------------------

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
lexLine (TqLex delim) src0 = case Src.evalParse lexDelim src0 of
  Just ((ws_m, delim'), rest) -> do
    let acc0 = delim' : (maybe [] (:[]) ws_m)
    drain acc0 rest
  Nothing -> ([MlContent src0], TqLex delim)
  where
  lexDelim :: Src.Parse (Maybe Token, Token)
  lexDelim = do
    ws <- Src.theConsumed $ Src.takeWhile (`T.elem` " \t")
    delim' <- Src.theConsumed $ Src.takePrefix delim
    let ws_m = if Src.null ws then Nothing else Just (Whitespace ws)
    pure (ws_m, MlClose delim'.span)
  drain revacc src = case lexAfterMlDelim src of
    Just (tok, rest) -> drain (tok : revacc) rest
    Nothing -> (reverse revacc, StdLex)

lexStd :: SrcText -> Maybe (Token, SrcText)
lexStd src | Src.null src = Nothing
lexStd src = case Src.evalParse parser src of
  Just (tok, rest) -> Just (tok, rest)
  Nothing -> internalError "coverage token parsing failed somehow"
  where
  parser = Src.withConsumedK $ do
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
        pure $ \dots -> Punctuation dots.span (Dots $ T.length dots.text)
      Punct (Colons _) -> do
        _ <- Src.takeWhile (== ':')
        pure $ \colons -> Punctuation colons.span (Colons $ T.length colons.text)
      Punct ty ->
        pure $ \p -> Punctuation (p.span) ty
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
        pure Whitespace
      Hash -> do
        _ <- Src.takeAll
        pure Comment
    -- illegal tokens
      Ill -> do
        _ <- Src.takeWhile ((== Ill) . classify)
        pure Illegal

---------------------
------ Symbols ------
---------------------

lexSymbol :: Src.Parse (SrcText -> Token)
lexSymbol = do
  _ <- Src.takeWhile isId
  pure Symbol
  where
  isId c = case classify c of
    Id0 -> True
    Num -> True
    Sgn _ -> True
    _ -> False

---------------------
------ Numbers ------
---------------------

lexNumber :: Sign -> Radix -> Text -> Src.Parse (SrcText -> Token)
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
  pure $ \src -> Number src.span
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

---------------------
------ Strings ------
---------------------

------ SQL Strings ------

lexSqString :: Src.Parse (SrcText -> Token)
lexSqString = do
  parts <- takeSqs
  closeQuote <- Src.tryN 1 $ \case
    "'" -> Right $ Just SqlQuote
    _ -> Left Nothing
  pure $ \src -> Str src.span SqlQuote parts closeQuote

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
  src <- Src.theConsumed $ Src.takeWhile1 (/= '\'')
  pure $ StdStr src

takeSqEsc :: Src.Parse StrToken
takeSqEsc = do
  (loc, _) <- Src.withSpan $ do
    _ <- Src.sat (== '\'')
    _ <- Src.sat (== '\'')
    pure ()
  pure $ StrEscape loc '\''

------ Double-quoted Strings ------

lexDqString :: QuoteType -> Src.Parse (SrcText -> Token)
lexDqString openQuote = do
  parts <- takeDqs
  closeQuote <- takeQuote
  pure $ \src -> Str src.span openQuote parts closeQuote

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
takeDq = Src.peeks (maybe SQuo classifyString) >>= \case
  SStd -> do
    src <- Src.theConsumed $ Src.takeWhile1 ((== SStd) . classifyString)
    pure . Just $ StdStr src
  SEsc -> Just <$> takeDqEsc
  SQuo -> pure Nothing
  SIll -> do
    src <- Src.theConsumed $ Src.takeWhile1 ((== SIll) . classifyString)
    pure . Just $ IllStr src

takeDqEsc :: Src.Parse StrToken
takeDqEsc = do
  (src, tokE) <- Src.withConsumed $ do
    _ <- Src.sat ((== SEsc) . classifyString)
    stdEsc <|> xEsc <|> uEsc <|> pure Nothing
  pure $ case tokE of
    Just code -> StrEscape src.span code
    Nothing -> IllStr src
  where
  stdEsc :: Src.Parse (Maybe Char)
  stdEsc = do
    -- escape character
    char <- Src.sat (`elem` (fst <$> stdEscapes))
    -- loookup meaning
    pure $ lookup char stdEscapes
  xEsc :: Src.Parse (Maybe Char)
  xEsc = do
    -- escape character
    _ <- Src.sat (== 'x')
    -- two digits
    aM <- (Just <$> Src.sat isHexDigit) <|> (Nothing <$ Src.take 1)
    bM <- (Just <$> Src.sat isHexDigit) <|> (Nothing <$ Src.take 1)
    -- parse digits or form error
    pure $ do
      a <- aM
      b <- bM
      code <- parseHex [a, b]
      Just (chr $ fromInteger code)
  uEsc :: Src.Parse (Maybe Char)
  uEsc = do
    -- escape character
    _ <- Src.sat (== 'u')
    -- open bracket, hex digits, close bracket
    openM <- (Just <$> Src.sat (== '{')) <|> pure Nothing
    codeM <- (Just <$> Src.takeWhile1 isHexDigit) <|> pure Nothing
    closeM <- (Just <$> Src.sat (== '}')) <|> pure Nothing
    -- parse digits or form error
    pure $ do
      _ <- openM
      digits <- codeM
      _ <- closeM
      code <- parseHex (T.unpack digits)
      unless (0 <= code && code <= 0x10FFFF) Nothing
      when (0xD000 <= code && code <= 0xDFFF) Nothing -- surrogate pairs are invalid codepoints
      Just (chr $ fromInteger code)

------ Multi-line Strings ------

lexTqStr :: Src.Parse (SrcText -> Token)
lexTqStr = do
  -- we've already checked for three quotes before
  -- now take remaining quotes
  _ <- Src.takeWhile (== '\"')
  _ <- Src.takeWhile (\c -> 'A' <= c && c <= 'Z'
                      || 'a' <= c && c <= 'z')
  pure MlDelim

lexAfterMlDelim :: SrcText -> Maybe (Token, SrcText)
lexAfterMlDelim src | Src.null src = Nothing
lexAfterMlDelim src = case Src.evalParse parser src of
  Just (tok, rest) -> Just (tok, rest)
  Nothing -> internalError "coverage token parsing failed somehow"
  where
  parser = Src.withConsumedK $ do
    c <- Src.sat (const True)
    case classify c of
    -- whitespace
      Lws -> do
        _ <- Src.takeWhile (`T.elem` " \t")
        pure Whitespace
      Hash -> do
        _ <- Src.takeAll
        pure Comment
    -- everything else is illegal
      _ -> do
        _ <- Src.takeWhile (not . (`T.elem` " \t#"))
        pure Illegal

--------------------------------
------ Terminals/Alphabet ------
--------------------------------

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

data ClassifyString
  = SStd
  | SQuo
  | SEsc
  | SIll
  deriving (Eq)

classifyString :: Char -> ClassifyString
classifyString c
  -- ascii control codes
  | '\0' <= c && c <= '\x1F' = SIll
  -- ascii printing
  | ' ' <= c && c <= '~' = case c of
    '\"' -> SQuo
    '`' -> SQuo
    '\\' -> SEsc
    _ -> SStd
  -- ascii delete and proper unicode
  | otherwise = SIll -- TODO allow at least some unicode characters, probably anything not a control code

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

---------------------
------ Helpers ------
---------------------

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

