{-# LANGUAGE ApplicativeDo #-} -- TOODO mark this extension as used

module Language.CCS.Parser
  ( CCS(..)
  , CST(..)
  , Atom(..)
  , parse
  , ParseError(..)
  ) where

import Control.Applicative (Alternative(..))
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.Location (Pos, Span, spanFromPos)
import Language.Nanopass (deflang, defpass)

import qualified Data.List.NonEmpty as NE
import qualified Language.CCS.Lexer.Sandhi as L0


-------------------
------ Types ------
-------------------

[deflang|
(CCS from L0:CCS
  (- Token)
  (- PunctuationType)

  (+ CST
    (Atom Span Atom)
    (Enclose Span Encloser (? CST))
    (Pair Span CST CST)
    (List Span Separator (+ CST))
    (Block Span (+ CST))
    (Template Span Text (+ (& CST Text)))
  )
)
|]
deriving instance Show CST
deriving instance Show Atom

data Encloser
  = Round
  | Square
  | Curly
  deriving (Eq, Show)

data Separator
  = Semicolon
  | Comma
  | Space
  | Chain
  | Qualify
  deriving (Eq, Show)

instance HasField "span" CST Span where
  getField (Atom a _) = a
  getField (Enclose a _ _) = a
  getField (Pair a _ _) = a
  getField (List a _ _) = a
  getField (Block a _) = a
  getField (Template a _ _) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

xlate :: L0.Atom -> Atom
xlate = descendAtomI XlateI
  { onAtomI = const Nothing
  }

---------------------
------ Grammar ------
---------------------

parse :: Pos -> [L0.Token] -> ([ParseError], Maybe [CST])
parse pos0 = runParser parseTopLevel (spanFromPos pos0)

parseTopLevel :: Parser [CST]
parseTopLevel = do
  csts <- parseCst `sepBy` nextline
  endP
  pure csts

parseCst :: Parser CST
parseCst = parseSemicolonCst

parseSimpleCsts :: Parser (NonEmpty CST)
parseSimpleCsts = someP parseChainCst

parseSimpleCst :: Parser CST
parseSimpleCst = do
  xs <- parseSimpleCsts
  pure $ case xs of
    x :| [] -> x
    _ -> List (spanNE xs) Space xs

parseSemicolonCst :: Parser CST
parseSemicolonCst = do
  before <- optional $ punctuation L0.Semicolon
  inner <- parseCommaCst `sepBy1` punctuation L0.Semicolon
  after <- optional $ punctuation L0.Semicolon
  pure $ case (before, inner, after) of
    (Nothing, x :| [], Nothing) -> x
    _ -> List (spanAround before inner after) Semicolon inner

parseCommaCst :: Parser CST
parseCommaCst = do
  before <- optional $ punctuation L0.Comma
  inner <- parsePairCst `sepBy1` punctuation L0.Comma
  after <- optional $ punctuation L0.Comma
  pure $ case (before, inner, after) of
    (Nothing, x :| [], Nothing) -> x
    _ -> List (spanAround before inner after) Comma inner

parsePairCst :: Parser CST
parsePairCst = do
  k <- parseSpaceCst
  v_m <- optional $ do
    _ <- punctuation L0.Pair
    v <- parseSpaceCst
    pure v
  pure $ case v_m of
    Nothing -> k
    Just v -> Pair (k.span <> v.span) k v

parseSpaceCst :: Parser CST
parseSpaceCst = do
  thisLine <- parseSimpleCsts
  contLine <- optional $ parseLineCont
  pure $ case thisLine `NE.appendList` fromMaybe [] contLine of
    x :| [] -> x
    xs -> List (spanNE xs) Space xs

parseLineCont :: Parser [CST]
parseLineCont = do
  _ <- punctuation L0.ContinueLine
  xss <- parseSimpleCsts `sepBy1` nextline
  pure $ concatMap NE.toList xss

parseChainCst :: Parser CST
parseChainCst = do
  xs <- parseQualCst `sepBy1` punctuation L0.Chain
  pure $ case xs of
    x :| [] -> x
    _ -> List (spanNE xs) Chain xs

parseQualCst :: Parser CST
parseQualCst = do
  xs <- parseBaseCst `sepBy1` punctuation L0.Qualify
  pure $ case xs of
    x :| [] -> x
    _ -> List (spanNE xs) Qualify xs

parseBaseCst :: Parser CST
parseBaseCst = parseAtom <|> parseEnclosed <|> parseTemplate

parseAtom :: Parser CST
parseAtom = satP $ \case
    L0.Atom spn atom -> Right $ Atom spn (xlate atom)
    it -> Left $ Expecting "atom" it.span

parseTemplate :: Parser CST
parseTemplate = do
  open <- template L0.OpenTemplate
  cst0 <- parseSimpleCst
  mids <- manyP $ do
    mid <- template L0.MidTemplate
    cst <- parseSimpleCst
    pure $
      let (_, txt) = mid
       in (txt, cst)
  close <- template L0.CloseTemplate
  pure $
    let (openSpan, openText) = open
        (closeSpan, closeText) = close
     in Template (openSpan <> closeSpan) openText (shmamle cst0 mids closeText)
  where
  shmamle :: CST -> [(Text, CST)] -> Text -> NonEmpty (CST, Text)
  shmamle cst0 [] end = (cst0, end) :| []
  shmamle cst0 ((txt, cst) : rest) end = (cst0, txt) <| shmamle cst rest end

parseEnclosed :: Parser CST
parseEnclosed
   =  parseEnclosedBy L0.Round
  <|> parseEnclosedBy L0.Square
  <|> parseEnclosedBy L0.Curly
  <|> parseBareIndent
  where
  parseEnclosedBy :: L0.BracketType -> Parser CST
  parseEnclosedBy ty = do
    open <- punctuation (L0.Open ty)
    inner <- optional $ parseCst <|> parseBracketedIndent
    close <- punctuation (L0.Close ty)
    pure $ Enclose (open <> close) Round inner

parseBareIndent :: Parser CST
parseBareIndent = do
  open <- punctuation L0.StartBlock
  _ <- indent
  inner <- parseCst `sepBy1` nextline
  _ <- dedent
  pure $ Block (open <> spanNE inner) inner

parseBracketedIndent :: Parser CST
parseBracketedIndent = do
  open <- indent
  inner <- parseCst `sepBy1` nextline
  close <- dedent
  pure $ Block (open <> close) inner

------ terminals ------

indent :: Parser Span
indent = satP $ \case
  L0.Indent spn -> Right spn
  it -> Left $ Expecting "indent" it.span

nextline :: Parser Span
nextline = satP $ \case
  L0.Nextline spn -> Right spn
  it -> Left $ Expecting "nextline" it.span

dedent :: Parser Span
dedent = satP $ \case
  L0.Dedent spn -> Right spn
  it -> Left $ Expecting "dedent" it.span

template :: L0.TemplateType -> Parser (Span, Text)
template ty = satP $ \case
  L0.StringTemplate spn ty' txt | ty == ty' -> Right (spn, txt)
  it -> Left $ Expecting errMsg it.span
  where
  errMsg = case ty of
    L0.OpenTemplate -> "open template"
    L0.MidTemplate -> "inner template"
    L0.CloseTemplate -> "close template"

punctuation :: L0.PunctuationType -> Parser Span
punctuation p = satP $ \case
  L0.Punctuation spn p' | p == p' -> Right spn
  it -> Left $ Expecting errMsg it.span
  where
  errMsg = case p of
    L0.Open L0.Round -> "open paren"
    L0.Open L0.Square -> "open square bracket"
    L0.Open L0.Curly -> "open curly brace"
    L0.Close L0.Round -> "close paren"
    L0.Close L0.Square -> "close square bracket"
    L0.Close L0.Curly -> "close curly brace"
    L0.StartBlock -> "colon"
    L0.Semicolon -> "semicolon"
    L0.Comma -> "comma"
    L0.Pair -> "colon"
    L0.Chain -> "chain (dot or adjacent open bracket)"
    L0.Qualify -> "colon"
    L0.ContinueLine -> "backslash"

------ Helpers ------

spanNE :: NonEmpty CST -> Span
spanNE xs = (NE.head xs).span <> (NE.last xs).span

spanAround :: Maybe Span -> NonEmpty CST -> Maybe Span -> Span
spanAround before xs after =
  fromMaybe (NE.head xs).span before <> fromMaybe (NE.last xs).span after

-------------------------------------------
------ The Parser Combinator Library ------
-------------------------------------------

-- We commit to a branch once we've consumed input.
-- We track mulltiple errors as we progress, to enable error recovery.

------ Core ------

newtype Parser a = P { unP :: St -> Result a }

data St = St
  { rest :: [L0.Token]
  , pos :: Span
  }

unconsSt :: St -> Maybe (L0.Token, St)
unconsSt st = case st.rest of
  t : x : xs -> Just (t, st{rest = x:xs, pos = x.span})
  [t] -> Just (t, st{rest = [], pos = spanFromPos t.span.end})
  [] -> Nothing

data Result a
  = Ok [ParseError] a (Maybe St)
  | Err [ParseError] (Maybe St)

deriving instance Functor Result
instance Applicative Result where
  pure x = Ok [] x Nothing
  Ok errs1 f st <*> Ok errs2 x st' = Ok (errs2 <> errs1) (f x) (st' <|> st)
  Ok errs1 _ st <*> Err errs2 st' = Err (errs2 <> errs1) (st' <|> st)
  Err errs1 st <*> _ = Err errs1 st
instance Monad Result where
  Ok errs1 x st >>= k = case k x of
    Ok errs2 y st' -> Ok (errs2 <> errs1) y (st' <|> st)
    Err errs2 st' -> Err (errs2 <> errs1) (st' <|> st)
  Err errs st >>= _ = Err errs st
withSt :: St -> Result a -> Result (St, a)
withSt st0 (Ok errs1 x st1) = Ok errs1 (fromMaybe st0 st1, x) st1
withSt _ (Err errs1 st1) = Err errs1 st1

data ParseError
  = UnexpectedEndOfInput Span
  | Expecting String Span

runParser :: Parser a -> Span -> [L0.Token] -> ([ParseError], Maybe a)
runParser p loc inp = case unP p St{ rest = inp, pos = loc } of
  Ok errs a _ -> (reverse errs, Just a)
  Err errs _ -> (reverse errs, Nothing)

instance Functor Parser where
  fmap f p = P $ \st -> f <$> unP p st
instance Applicative Parser where
  pure x = P $ \_ -> pure x
  (<*>) = seqP
instance Alternative Parser where
  empty = P $ \_ -> Err [] Nothing -- FIXME
  a <|> b = altP a b
-- intentionally no monad instance

------ Primitive Combinators ------

--- Units ---

-- Take a single (terminal) token matching the predicate.
-- Thus, includes a single specific token (equals), take any token (const True) and always fail (const False).
satP :: (L0.Token -> Either ParseError a) -> Parser a
satP p = P $ \st -> case unconsSt st of
  Just (x, st') -> case p x of
    Right y -> Ok [] y (Just st')
    Left err -> Err [err] Nothing
  Nothing -> Err [UnexpectedEndOfInput st.pos] Nothing

-- Detect end of input
endP :: Parser ()
endP = P $ \st -> case unconsSt st of
  Nothing -> Ok [] () Nothing
  Just (x, _) -> Err [Expecting "end of input" x.span] Nothing

--- Sequencing ---
seqP :: Parser (a -> b) -> Parser a -> Parser b
seqP getF getX = P $ \st0 -> do
  (st1, f) <- withSt st0 $ unP getF st0
  x <- unP getX st1
  pure $ f x

--- Branching ---
altP :: Parser a -> Parser a -> Parser a
altP a b = P $ \st -> case unP a st of
  ok@(Ok _ _ _) -> ok
  err@(Err _ st') -> case st' of
    Nothing -> err >> unP b st
    Just _ -> err

-- Backtracking
try :: Parser a -> Parser a
try p = P $ \st -> case unP p st of
  ok@(Ok _ _ _) -> ok
  Err errs1 _ -> Err errs1 Nothing

--- Error Management ---

-- Recovery
recoverP :: Parser a -> a -> Parser a
recoverP p x = P $ \st -> case unP p st of
  ok@(Ok _ _ _) -> ok
  Err errs1 _ -> Ok errs1 x Nothing

infixl 3 <??>
(<??>) :: Parser a -> a -> Parser a
(<??>) = recoverP

-- Labelling/Condensing

------ Derived Combinators ------

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

manyP :: Parser a -> Parser [a]
manyP p = loop
  where
  loop = more <|> pure []
  more = do
    x <- p
    xs <- loop
    pure $ x : xs

someP :: Parser a -> Parser (NonEmpty a)
someP p = (:|) <$> p <*> manyP p

sepBy :: Parser a -> Parser any -> Parser [a]
sepBy p sep = start <|> pure []
  where
  start = do
    x <- p
    xs <- loop
    pure $ x : xs
  loop = more <|> pure []
  more = do
    _ <- sep
    x <- p
    xs <- loop
    pure $ x : xs

sepBy1 :: Parser a -> Parser any -> Parser (NonEmpty a)
sepBy1 p sep = start
  where
  start = do
    x <- p
    xs <- loop
    pure $ x :| xs
  loop = more <|> pure []
  more = do
    _ <- sep
    x <- p
    xs <- loop
    pure $ x : xs

