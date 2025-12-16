module Language.CCS.Lexer.Sandhi.Indentation
  ( CCS(..)
  , Token(..)
  , annotation
  , process
  , MalformedIndentation(..)
  ) where

import Prelude hiding (lines)

import Control.Monad (when, unless, replicateM_)
import Data.Text (Text)
import Language.CCS.Error (internalError, unwrapOrPanic_)
import Language.Location (incCol, Span, mkSpan)
import Language.Nanopass (deflang, defpass)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Data.Text as T
import qualified Language.CCS.Lexer.Assemble.Strings as L0
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
((CCS loc) from L0:CCS
  (* Token
    (- Indentation)
    (+ Indent loc)
    (+ Nextline loc)
    (+ Dedent loc)
    (- MultilineLiteral)
    (+ MultilineLiteral loc (* MlLine))
  )
  (* MlLine
    (- MlLine)
    (+ MlLine (& loc Text))
  )
)
|]

deriving instance Show a => Show (Token a)
deriving instance Show a => Show (MlLine a)
deriving instance Functor Token
deriving instance Functor MlLine

annotation :: Token a -> a
annotation (Symbol a _) = a
annotation (Punctuation a _) = a
annotation (Indent a) = a
annotation (Nextline a) = a
annotation (Dedent a) = a
annotation (Whitespace a _) = a
annotation (IntegerLiteral a _) = a
annotation (FloatingLiteral a _) = a
annotation (StringLiteral a _) = a
annotation (MultilineLiteral a _) = a


$(pure [])

[defpass|(from L0:CCS to CCS)|]

xlate :: L0.Token loc -> Token loc
xlate = descendTokenI XlateI
  { onTokenI = const Nothing
  , onMlLineI = const Nothing
  , onTokenIndentationI = \_ _ -> internalError "attempt to translate Indentation token to next lexing stage"
  , onTokenMultilineLiteralI = \_ _ _ -> internalError "attempt to translate MultilineLiteral to next lexing stage"
  , onMlLineMlLineI = \_ _ -> internalError "attempt to translate MlLine to next lexing stage"
  }

------ Main ------

process ::
  ( MalformedIndentation m
  )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
process = findFirstUnindented . sanitize

-- This one removes duplicate indentations (ie blank lines)
sanitize ::
  ( MalformedIndentation m
  )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (L0.Token Span)) m r
sanitize inp0 = S.effect $ S.next inp0 >>= \case
  Right (i1@(L0.Indentation _ _), inp1) -> S.next inp1 >>= \case
    Right (i2@(L0.Indentation _ _), inp2) -> pure $ do
      let rest = yield i2 >> inp2
      sanitize rest
    Right (other, inp2) -> pure $ do
      yield i1
      let rest = yield other >> inp2
      sanitize rest
    Left r -> pure $ yield i1 >> pure r
  Right (other, rest) -> pure $ do
    yield other
    sanitize rest
  Left r -> pure $ pure r

detectIndentation ::
  ( MalformedIndentation m
  )
  => IndentState
  -> Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
detectIndentation st inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Indentation l ws, inp1) -> pure $ do
    newLevel <- S.effect $ analyzeIndent st (l, ws)
    detectIndentation (newLevel, snd st) inp1
  Right (L0.MultilineLiteral l lines preDelim, inp1) -> do
    lines' <- mapM (xlateMl $ indentString st) lines
    when (snd preDelim /= indentString st) $ do
      raiseBadWhitespaceBeforeMultiLineDelimiter (fst preDelim) (indentString st)
    pure $ do
      yield $ MultilineLiteral l lines'
      detectIndentation st inp1
  Right (other, inp1) -> pure $ do
    yield $ xlate other
    detectIndentation st inp1
  Left r -> pure $ pure r

analyzeIndent :: MalformedIndentation m
  => IndentState
  -> (Span, Text)
  -> m (Stream (Of (Token Span)) m Int)
analyzeIndent (lvl, ty) (l, ws) = do
  (lIndent, newLvl) <- go ty
  pure $ if
    | newLvl == lvl -> do
      yield $ Nextline lIndent
      pure lvl
    | newLvl == lvl + 1 -> do
      yield $ Indent lIndent
      pure newLvl
    | newLvl < lvl -> do
      replicateM_ (lvl - newLvl) $ do
        yield $ Dedent lIndent
      pure newLvl
    | otherwise -> -- indent deeper than n + 1
      internalError "length of leading tabs is not let, eq, or one more than current tab state"
  where
  go (Spaces n) = do
        -- compute which whitespace is valid indent and which isn't, also the new level
    let (allSpaces, rest2) = T.span (== ' ') ws -- take only spaces from the front
        (maxSpaces, rest1) = T.splitAt (n * (lvl + 1)) allSpaces -- then clamp to the maximum indent
        (newLvl, extraSpaces)  = T.length maxSpaces `divMod` n
        (spaces, rest0) = T.splitAt (T.length maxSpaces - extraSpaces) maxSpaces -- then remove any spaces that represent a partial indent
        rest = rest0 <> rest1 <> rest2
        -- compute location between indentation and leading whitespace
        lMid = T.foldl' (\c _ -> incCol c) l.start spaces
        lIndent = unwrapOrPanic_ $ mkSpan l.start lMid
    unless (T.null rest) $ do
      let lRest = unwrapOrPanic_ $ mkSpan lMid l.end
      raiseLeadingWhitespace lRest
    pure (lIndent, newLvl)
  go Tab = do
        -- compute which whitespace is valid indent and which isn't, also the new level
    let (allTabs, rest1) = T.span (== '\t') ws -- take only tabs from the front
        (tabs, rest0) = T.splitAt (lvl + 1) allTabs -- then clamp to the maximum indent
        rest = rest0 <> rest1
        newLvl = T.length tabs
        -- compute location between indentation and leading whitespace
        lMid = T.foldl' (\c _ -> incCol c) l.start tabs
        lIndent = unwrapOrPanic_ $ mkSpan l.start lMid
    unless (T.null rest) $ do
      let lRest = unwrapOrPanic_ $ mkSpan lMid l.end
      raiseLeadingWhitespace lRest
    pure (lIndent, newLvl)

------ Initialization ------

findFirstUnindented ::
  ( MalformedIndentation m )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
findFirstUnindented inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Indentation _ "", inp1) -> pure $ findFirstIndented inp1
  Right (L0.Indentation l _, inp1) -> do
    raiseUnexpectedIndent l
    pure $ findFirstUnindented inp1
  Right (L0.MultilineLiteral l lines preDelim, inp1) -> do
    lines' <- mapM (xlateMl "") lines
    when (snd preDelim /= "") $ do
      raiseBadWhitespaceBeforeMultiLineDelimiter (fst preDelim) ""
    pure $ do
      yield $ MultilineLiteral l lines'
      findFirstUnindented inp1
  Right (other, inp1) -> pure $ do
    yield $ xlate other
    findFirstUnindented inp1
  Left r -> pure $ pure r

findFirstIndented ::
  ( MalformedIndentation m )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
findFirstIndented inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Indentation l "", inp1) -> pure $ do
    yield $ Nextline l
    findFirstIndented inp1
  Right (L0.Indentation l ws, inp1) -> do
    indentTy <- case getIndentType (l, ws) of
      (ty, _, T.Empty) -> pure ty
      (ty, l', _) -> do
        raiseLeadingWhitespace l'
        pure ty
    pure $ do
      yield $ Indent l
      detectIndentation (1, indentTy) inp1
  Right (L0.MultilineLiteral l lines preDelim, inp1) -> do
    lines' <- mapM (xlateMl "") lines
    when (snd preDelim /= "") $ do
      raiseBadWhitespaceBeforeMultiLineDelimiter (fst preDelim) ""
    pure $ do
      yield $ MultilineLiteral l lines'
      findFirstUnindented inp1
  Right (other, inp1) -> pure $ do
    yield $ xlate other
    findFirstIndented inp1
  Left r -> pure $ pure r

------ Helpers ------

type IndentState = (Int, IndentType)

data IndentType
  = Tab
  | Spaces Int

getIndentType :: (Span, Text) -> (IndentType, Span, Text)
getIndentType (_, "") = internalError "detecting indentation over empty string"
getIndentType (l, '\t' T.:< rest) =
  let l' = unwrapOrPanic_ $ mkSpan (incCol l.start) l.end
   in (Tab, l', rest)
getIndentType (l, txt@(' ' T.:< _)) =
  let (spaces, rest) = T.span (== ' ') txt
      l' = unwrapOrPanic_ $ mkSpan (T.foldl' (\c _ -> incCol c) l.start spaces) l.end
   in (Spaces $ T.length spaces, l', rest)
getIndentType _ = internalError "non-empty indentation does not start with space or tab"

indentString :: IndentState -> Text
indentString (n, Tab) = T.replicate n "\t"
indentString (n, Spaces m) = T.replicate (n * m) " "

xlateMl ::
  ( MalformedIndentation m )
  => Text -- ^ leading indent
  -> L0.MlLine Span
  -> m (MlLine Span)
xlateMl indent (L0.MlLine (lWs, ws) (lTxt, txt)) = case T.stripPrefix indent ws of
  Nothing -> do
    raiseInsufficientIndentation lWs
    pure $ MlLine (lTxt, txt)
  Just wsTxt -> do
    let rIndent = T.foldl' (\c _ -> incCol c) lWs.start indent
        newSpan = unwrapOrPanic_ $ mkSpan rIndent lTxt.end
    pure $ MlLine (newSpan, wsTxt <> txt)

------ Errors ------

class Monad m => MalformedIndentation m where
  raiseUnexpectedIndent :: Span -> m ()
  raiseInsufficientIndentation :: Span -> m ()
  raiseBadWhitespaceBeforeMultiLineDelimiter :: Span -> Text -> m ()
  raiseLeadingWhitespace :: Span -> m ()
