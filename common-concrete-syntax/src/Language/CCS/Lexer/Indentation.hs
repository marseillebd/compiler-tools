module Language.CCS.Lexer.Indentation
  ( CCS(..)
  , Atom(..)
  , Token(..)
  , PunctuationType(..)
  , process
  , MalformedIndentation(..)
  ) where

import Prelude hiding (lines, init)

import Control.Applicative ((<|>))
import Control.Monad (forM, unless, replicateM_, void)
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.CCS.Error (internalError, unused, unwrapOrPanic_)
import Language.Location (Span, spanFromPos)
import Language.Nanopass (deflang, defpass)
import Language.Text (SrcText)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Data.Text as T
import qualified Language.CCS.Lexer.Assemble as L0
import qualified Language.Text as Src
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
(CCS from L0:CCS
  (* Atom
    (- MultilineLiteral)
    (+ MultilineLiteral (* SrcText))
  )

  (* Token
    (- Eol)
    (+ Indent Span)
    (+ Nextline Span)
    (+ Dedent Span)
  )
)
|]

deriving instance Show Atom
deriving instance Show Token
deriving instance Show PunctuationType

instance HasField "span" Token Span where
  getField (Atom a _) = a
  getField (StringTemplate a _ _) = a
  getField (Punctuation a _) = a
  getField (Whitespace a) = a.span
  getField (Indent a) = a
  getField (Nextline a) = a
  getField (Dedent a) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

_ignore :: ()
_ignore = unused (XlateI, descendAtomI, descendTokenI, descendPunctuationTypeI)

xlate :: MalformedIndentation m => IndentState -> L0.Token -> m Token
xlate st = descendToken Xlate
  { onAtom = const Nothing
  , onToken = const Nothing
  , onPunctuationType = const Nothing
  , onAtomMultilineLiteral = \body predelim -> xlateMl st body predelim
  , onTokenEol = \_ -> internalError "attempt to translate Eol to next lexing stage"
  }

------------------
------ Main ------
------------------

process ::
  ( MalformedIndentation m
  )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
process inp0 = do
  inp1 <- findFirstUnindented inp0
  findFirstIndented inp1 >>= \case
    Right (st0, inp2) -> detectIndentation st0 inp2
    Left r -> pure r

-------------------------
------ Indentation ------
-------------------------

detectIndentation ::
  ( MalformedIndentation m
  )
  => IndentState
  -> Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
detectIndentation st inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Eol eolLoc _, inp1) -> S.next inp1 >>= \case
    Right (L0.Whitespace ws, rest) -> pure $ do
      newLvl <- S.effect $ analyzeIndent st ws
      detectIndentation (newLvl, snd st) rest
    Right (L0.Eol _ _, _) -> internalError "found eol at start of file"
    Right (other, inp2) -> pure $ do
      let ws = Src.fromPos other.span.start ""
          rest = yield other >> inp2
      newLvl <- S.effect $ analyzeIndent st ws
      detectIndentation (newLvl, snd st) rest
    Left r -> pure $ do
      replicateM_ (fst st) $
        yield $ Dedent eolLoc
      pure r
  Right (other, rest) -> do
    other' <- xlate st other
    pure $ do
      yield other'
      detectIndentation st rest
  Left _ -> internalError "no eol at end of token stream"

analyzeIndent :: MalformedIndentation m
  => IndentState
  -> SrcText
  -> m (Stream (Of Token) m Int)
analyzeIndent (lvl, ty) ws = do
  let takeOneLvl = case ty of
        Spaces n -> void $ Src.takePrefix (T.replicate n " ")
        Tab -> void $ Src.sat (== '\t')
  let (indent, newLvl, rest) = unwrapOrPanic_ $ Src.runParse ws $
        length <$> Src.manyN (lvl + 1) takeOneLvl
  unless (Src.null rest) $ do
    raiseLeadingWhitespace rest.span
  pure $ if
    | newLvl == lvl -> do
      yield $ Nextline indent.span
      pure lvl
    | newLvl == lvl + 1 -> do
      yield $ Indent indent.span
      pure newLvl
    | newLvl < lvl -> do
      replicateM_ (lvl - newLvl) $ do
        yield $ Dedent indent.span
      yield $ Nextline indent.span
      pure newLvl
    | otherwise -> -- indent deeper than n + 1
      internalError "length of leading tabs is not le, eq, or one more than current tab state"

------ Initialization ------

findFirstUnindented ::
  ( MalformedIndentation m )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m (Stream (Of L0.Token) m r)
findFirstUnindented = init
  where
  init inp0 = S.effect $ S.next inp0 >>= \case
    Right (L0.Whitespace ws, rest) -> do
      raiseUnexpectedIndent ws.span
      pure $ loop rest
    Right (L0.Eol _ _, _) -> internalError "found eol at start of file"
    Right (other, inp1) -> pure $
      pure $ yield other >> inp1
    Left r -> pure $ pure $ pure r
  loop inp0 = S.effect $ S.next inp0 >>= \case
    Right (L0.Eol _ _, inp1) -> S.next inp1 >>= \case
      Right (L0.Whitespace ws, rest) -> do
        raiseUnexpectedIndent ws.span
        pure $ loop rest
      Right (L0.Eol _ _, _) -> internalError "found eol after eol"
      Right (other, inp2) -> pure $
        pure $ yield other >> inp2
      Left r -> pure $ pure $ pure r
    Right (other, rest) -> do
      other' <- xlate unknownIndent other
      pure $ do
        yield other'
        loop rest
    Left r -> pure $ pure $ pure r

findFirstIndented ::
  ( MalformedIndentation m )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m (Either r (IndentState, Stream (Of L0.Token) m r))
findFirstIndented inp0 = S.effect $ S.next inp0 >>= \case
  -- found it
  Right (L0.Eol _ _, inp1) -> S.next inp1 >>= \case
    Right (L0.Whitespace ws, rest) -> do
      let (okWs, ty, badWs) = getIndentType ws
      unless (Src.null badWs) $
        raiseLeadingWhitespace badWs.span
      pure $ do
        yield $ Indent okWs.span
        pure $ Right ((1, ty), rest)
  -- another unindented line
    Right (other, rest) -> do
      other' <- xlate unknownIndent other
      pure $ do
        yield $ Nextline (spanFromPos other.span.start)
        yield other'
        findFirstIndented rest
  -- base cases
    Left r -> pure $ pure $ Right (unknownIndent, pure r)
  Right (other, rest) -> do
    other' <- xlate unknownIndent other
    pure $ do
      yield other'
      findFirstIndented rest
  Left r -> pure $ pure $ Left r

getIndentType :: SrcText -> (SrcText, IndentType, SrcText)
getIndentType src = case Src.runParse src detect of
  Just (ok, ty, rest) -> (ok, ty, rest)
  Nothing -> internalError "detecting indentation over empty string"
  where
  detect
     =  (Src.sat (== '\t') >> pure Tab)
    <|> (Src.takeWhile1 (== ' ') >>= \it -> pure $ Spaces (T.length it))

--------------------------------
------ Multiline Literals ------
--------------------------------

xlateMl :: forall m.
  ( MalformedIndentation m )
  => IndentState
  -> [SrcText]
  -> SrcText
  -> m Atom
xlateMl (0, _) body predelim = do
  unless (Src.null predelim) $
    raiseUnexpectedIndent predelim.span
  pure $ MultilineLiteral body
xlateMl st body predelim = do
  body' <- forM body stripIndent
  predelim' <- stripIndent predelim
  unless (Src.null predelim') $
    raiseLeadingWhitespace predelim'.span
  pure $ MultilineLiteral body'
  where
  stripIndent :: SrcText -> m SrcText
  stripIndent src = case indentStripper src of
    Right (_, rest) -> pure rest
    Left (tooLittle, rest) -> do
      raiseInsufficientIndentation tooLittle.span
      pure rest
  indentStripper :: SrcText -> Either (SrcText, SrcText) (SrcText, SrcText)
  indentStripper src = case Src.execParse src $
    Src.takePrefix (indentString st) of
      Just it -> Right it
      Nothing -> Left $ unwrapOrPanic_ $ Src.execParse src $
        Src.takeWhile (== (indentChar st))

---------------------
------ Helpers ------
---------------------

type IndentState = (Int, IndentType)

data IndentType
  = Tab
  | Spaces Int

unknownIndent :: IndentState
unknownIndent = (0, undefined) -- the indent type doesn't matter if the level is zero

indentString :: IndentState -> Text
indentString (n, Tab) = T.replicate n "\t"
indentString (n, Spaces m) = T.replicate (n * m) " "

indentChar :: IndentState -> Char
indentChar (_, Tab) = '\t'
indentChar (_, Spaces _) = ' '

--------------------
------ Errors ------
--------------------

class Monad m => MalformedIndentation m where
  raiseUnexpectedIndent :: Span -> m ()
  raiseInsufficientIndentation :: Span -> m ()
  raiseLeadingWhitespace :: Span -> m ()
