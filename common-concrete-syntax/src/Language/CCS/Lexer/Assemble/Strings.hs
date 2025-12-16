module Language.CCS.Lexer.Assemble.Strings
  ( CCS(..)
  , Token(..)
  , StrLit(..)
  , StringType(..)
  , MlLine(..)
  , annotation
  , assemble
  , MalformedString(..)
  ) where

import Data.Text (Text)
import Language.CCS.Error (internalError, unwrapOrPanic_)
import Language.CCS.Lexer.Morpheme (QuoteType(..))
import Language.Location (Pos, Span, spanFromPos, mkSpan)
import Language.Nanopass (deflang, defpass)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Data.Text as T
import qualified Language.CCS.Lexer.Assemble.Numbers as L0
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
((CCS loc) from L0:CCS
  (* Token
    (- Quote)
    (- StdStr)
    (- StrEscape)
    (+ StringLiteral loc StrLit)
    (+ MultilineLiteral loc (* MlLine) (& loc Text))
  )
  (+ MlLine
    (MlLine
      (& loc Text)
      (& loc Text)
    )
  )
)
|]

data StrLit = StrLit
  { strTy :: StringType
  , strContent :: Text
  }
  deriving (Show)

data StringType
  = StrLiteral
  | OpenTemplate
  | MidTemplate
  | CloseTemplate
  deriving (Show)

deriving instance Show a => Show (Token a)
deriving instance Show a => Show (MlLine a)
deriving instance Functor Token
deriving instance Functor MlLine

annotation :: Token a -> a
annotation (Symbol a _) = a
annotation (Punctuation a _) = a
annotation (Whitespace a _) = a
annotation (Indentation a _) = a
annotation (IntegerLiteral a _) = a
annotation (FloatingLiteral a _) = a
annotation (StringLiteral a _) = a
annotation (MultilineLiteral a _ _) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

xlate :: L0.Token loc -> Token loc
xlate = descendTokenI XlateI
  { onTokenI = const Nothing
  , onTokenQuoteI = \_ _ -> internalError "attempt to translate Quote token to next lexing stage"
  , onTokenStdStrI = \_ _ -> internalError "attempt to translate StdStr token to next lexing stage"
  , onTokenStrEscapeI = \_ _ -> internalError "attempt to translate StrEscape token to next lexing stage"
  }

assemble ::
  ( Monad m
  , MalformedString m
  )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
assemble = stdMode

stdMode ::
  ( Monad m
  , MalformedString m
  )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
stdMode inp0 = S.effect $ S.next inp0 >>= \case
  Left r -> pure $ pure r
  Right (L0.Quote l (MlQuote _), rest) -> pure $ do
    let st = MlSt
          { mlL = l.start
          , mlLinesReverse = []
          , mlR = l.end
          }
    mlMode st rest
  Right (L0.Quote l ty, rest) -> pure $ do
    let st = StrSt
          { strL = l.start
          , strOpenTy = ty
          , strTxt = ""
          , strR = l.end
          }
    strMode st rest
  Right (L0.StdStr _ _, _) ->
    internalError "string part without open quote"
  Right (L0.StrEscape _ _, _) ->
    internalError "string escape without open quote"
  Right (other, rest) -> pure $ do
    yield $ xlate other
    stdMode rest

data StrSt = StrSt
  { strL :: Pos -- ^ position to the left of the open quote mark
  , strOpenTy :: QuoteType -- ^ type of the open quote
  , strTxt :: Text -- ^ accumulated string content
  , strR :: Pos -- ^ position to the right of the string content so far (or the open quote)
  }

strMode ::
  ( Monad m
  , MalformedString m
  )
  => StrSt
  -> Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
strMode st inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Quote _ (MlQuote _), _) ->
    internalError "standard string closed by triple quote"
  Right (L0.Quote l ty, rest) -> pure $ do
    yieldStr $ Just (ty, l.end)
    stdMode rest
  Right (L0.StdStr l txt, rest) -> pure $ do
    strMode st
      { strTxt = st.strTxt <> txt
      , strR = l.end
      } rest
  Right (L0.StrEscape l c, rest) -> pure $ do
    strMode st
      { strTxt = st.strTxt <> T.singleton c -- TODO multiple escapes together might encode a valid text, but this implementation may corrupt them as they add the codepoints one-by-one
      , strR = l.end
      } rest
  Right (other, inp1) -> do
    raiseExpectingCloseQuote (L0.annotation other)
    pure $ do
      yieldStr Nothing
      let rest = yield other >> inp1
      stdMode rest
  Left r -> do
    raiseExpectingCloseQuote (spanFromPos st.strR)
    pure $ pure r
  where
  yieldStr :: Monad m => Maybe (QuoteType, Pos) -> Stream (Of (Token Span)) m ()
  yieldStr close = do
    let tok = StrLit
          { strTy = mkStrType st.strOpenTy (fst <$> close)
          , strContent = st.strTxt
          }
        spn = unwrapOrPanic_ $ mkSpan st.strL (maybe st.strR snd close)
    yield $ StringLiteral spn tok

mkStrType :: QuoteType -> Maybe QuoteType -> StringType
mkStrType SqlQuote _ = StrLiteral
mkStrType DblQuote (Just Backtick) = OpenTemplate
mkStrType DblQuote _ = StrLiteral
mkStrType Backtick (Just Backtick) = MidTemplate
mkStrType Backtick _ = CloseTemplate
mkStrType (MlQuote _) _ = internalError "mkStrType called on an MlQuote"

data MlSt = MlSt
  { mlL :: Pos -- ^ position to the left of the open quotes
  , mlLinesReverse :: [MlLine Span]
  , mlR :: Pos -- ^ position to the right of the string content so far (or the open quote)
  }

mlMode ::
  ( Monad m
  , MalformedString m
  )
  => MlSt
  -> Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
mlMode st inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Indentation lWs ws, inp1) -> S.next inp1 >>= \case
    Right (L0.StdStr l txt, rest) -> pure $ do
      let st' = addLine (Just (lWs, ws)) (Just (l, txt))
      mlMode st' rest
    Right (L0.Quote l (MlQuote _), rest) -> pure $ do
      yieldStr $ Just (lWs, ws, l.end)
      stdMode rest
    Right (L0.Quote _ _, _) -> internalError "standard quote inside muliline literal"
    Right (other, inp2) -> pure $ do
      let st' = addLine (Just (lWs, ws)) Nothing
          rest = yield other >> inp2
      mlMode st' rest
    Left r -> pure $ do
      let st' = addLine (Just (lWs, ws)) Nothing
          rest = pure r
      mlMode st' rest
  Right (L0.StdStr l txt, rest) -> pure $ do
    let st' = addLine Nothing (Just (l, txt))
    mlMode st' rest
  Right (L0.Quote l (MlQuote _), rest) -> pure $ do
    yieldStr $ Just (spanFromPos l.start, "", l.end)
    stdMode rest
  Right (L0.Quote _ _, _) -> internalError "standard quote inside muliline literal"
  Right (L0.StrEscape _ _, _) -> internalError "string escape inside multiline literal"
  Right (other, inp1) -> do
    raiseExpectingCloseQuote (L0.annotation other)
    pure $ do
      let rest = yield other >> inp1
      yieldStr Nothing
      stdMode rest
  Left r -> do
    raiseExpectingCloseQuote (spanFromPos st.mlR)
    pure $ do
      yieldStr Nothing
      pure r
  where
  addLine
    :: Maybe (Span, Text) -- ^ leading whitespace
    -> Maybe (Span, Text) -- ^ line content
    -> MlSt
  addLine (Just ws) (Just txt) = goLine ws txt
  addLine (Just (lWs, ws)) Nothing = goLine (lWs, ws) (spanFromPos lWs.end, "")
  addLine Nothing (Just (l, txt)) = goLine (spanFromPos l.start, "") (l, txt)
  addLine Nothing Nothing = goLine (spanFromPos st.mlR, "") (spanFromPos st.mlR, "")
  goLine :: (Span, Text) -> (Span, Text) -> MlSt
  goLine (lWs, ws) (l, txt) =
    let line' = MlLine (lWs, ws) (l, txt)
     in st
          { mlLinesReverse = line' : st.mlLinesReverse
          , mlR = l.end
          }

  yieldStr :: Monad m => Maybe (Span, Text, Pos) -> Stream (Of (Token Span)) m ()
  yieldStr Nothing = do
    let spn = unwrapOrPanic_ $ mkSpan st.mlL st.mlR
        preDelim = (spanFromPos st.mlR, "")
    yield $ MultilineLiteral spn (reverse st.mlLinesReverse) preDelim
  yieldStr (Just (lWs, ws, rDelim)) = do
    let spn = unwrapOrPanic_ $ mkSpan st.mlL rDelim
        preDelim = (lWs, ws)
    yield $ MultilineLiteral spn (reverse st.mlLinesReverse) preDelim


class MalformedString m where
  raiseExpectingCloseQuote :: Span -> m ()
