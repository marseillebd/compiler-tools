module Language.CCS.Lexer.Indentation
  ( CCS(..)
  , Token(..)
  , Atom(..)
  , PunctuationType(..)
  , process
  ) where

import Prelude hiding (lines, init)

import Language.CCS.Types.Indentation

import Control.Applicative ((<|>))
import Control.Monad (forM, unless, replicateM_, void)
import Data.Text (Text)
import Language.CCS.Error (ReaderHooks(..), ReaderError(..))
import Language.CCS.Util (internalError, unused, unwrapOrPanic_)
import Language.Location (spanFromPos)
import Language.Nanopass (defpass)
import Language.Text (SrcText)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Data.Text as T
import qualified Language.CCS.Types.Assemble as L0
import qualified Language.Text as Src
import qualified Streaming as S
import qualified Streaming.Prelude as S

[defpass|(from L0:CCS to CCS)|]

_ignore :: ()
_ignore = unused (XlateI, descendAtomI, descendTokenI, descendPunctuationTypeI)

xlate :: ReaderHooks m => IndentState -> L0.Token -> m Token
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
  ( ReaderHooks m
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
  ( ReaderHooks m
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
  Left r -> pure $ pure r

analyzeIndent :: ReaderHooks m
  => IndentState
  -> SrcText
  -> m (Stream (Of Token) m Int)
analyzeIndent (lvl, ty) ws = do
  let ((indent, newLvl), rest) = unwrapOrPanic_ $ Src.evalParse parseIndent ws
  unless (Src.null rest) $ do
    recoverableError $ LeadingWhitespace rest
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
  where
  parseIndent :: Src.Parse (SrcText, Int)
  parseIndent = Src.withConsumed $ do
    length <$> Src.manyN (lvl + 1) parseOneLvl
  parseOneLvl :: Src.Parse ()
  parseOneLvl = case ty of
    Spaces n -> void $ Src.takePrefix (T.replicate n " ")
    Tab -> void $ Src.sat (== '\t')

------ Initialization ------

findFirstUnindented ::
  ( ReaderHooks m )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m (Stream (Of L0.Token) m r)
findFirstUnindented = init
  where
  init inp0 = S.effect $ S.next inp0 >>= \case
    Right (L0.Whitespace ws, rest) -> do
      recoverableError $ LeadingWhitespaceBeforeFirstIndent ws
      pure $ loop rest
    Right (L0.Eol _ _, _) -> internalError "found eol at start of file"
    Right (other, inp1) -> pure $
      pure $ yield other >> inp1
    Left r -> pure $ pure $ pure r
  loop inp0 = S.effect $ S.next inp0 >>= \case
    Right (L0.Eol _ _, inp1) -> S.next inp1 >>= \case
      Right (L0.Whitespace ws, rest) -> do
        recoverableError $ LeadingWhitespaceBeforeFirstIndent ws
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
  ( ReaderHooks m )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m (Either r (IndentState, Stream (Of L0.Token) m r))
findFirstIndented inp0 = S.effect $ S.next inp0 >>= \case
  -- found it
  Right (L0.Eol _ _, inp1) -> S.next inp1 >>= \case
    Right (L0.Whitespace ws, rest) -> do
      let (okWs, ty, badWs) = getIndentType ws
      unless (Src.null badWs) $
        recoverableError $ MixedWhitespaceInIndentation badWs
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
getIndentType src = case Src.evalParse detect src of
  Just ((ok, ty), rest) -> (ok, ty, rest)
  Nothing -> internalError "detecting indentation over empty string"
  where
  detect = Src.withConsumed
     $  (Src.sat (== '\t') >> pure Tab)
    <|> (Src.takeWhile1 (== ' ') >>= \it -> pure $ Spaces (T.length it))

--------------------------------
------ Multiline Literals ------
--------------------------------

xlateMl :: forall m.
  ( ReaderHooks m )
  => IndentState
  -> [SrcText]
  -> SrcText
  -> m Atom
xlateMl (0, _) body predelim = do
  unless (Src.null predelim) $
    recoverableError $ ExcessIndentationBeforeMultilineClose predelim
  pure $ MultilineLiteral body
xlateMl st body predelim = do
  body' <- forM body stripIndent
  predelim' <- stripIndent predelim
  unless (Src.null predelim') $
    recoverableError $ ExcessIndentationBeforeMultilineClose predelim'
  pure $ MultilineLiteral body'
  where
  stripIndent :: SrcText -> m SrcText
  stripIndent src = case Src.evalParse parseIndent src of
    Just (Right (), rest) -> pure rest
    Just (Left tooLittle, rest) -> do
      recoverableError $ TooLittleIndentationInMultiline
        { theTooLittleIndentation = tooLittle
        -- , multilineStartsWith = delim
        , expectedIndentation = indentString st
        }
      pure rest
    Nothing -> internalError "xlateMl.stripIndent failed"
  parseIndent :: Src.Parse (Either SrcText ())
  parseIndent
     = Right () <$ Src.takePrefix (indentString st)
    <|> Left <$> Src.theConsumed (Src.takeWhile (== (indentChar st)))

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
