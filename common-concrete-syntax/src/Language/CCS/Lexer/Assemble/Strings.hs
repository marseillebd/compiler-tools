module Language.CCS.Lexer.Assemble.Strings
  ( CCS(..)
  , Token(..)
  , StrLit(..)
  , StringType(..)
  , annotation
  , assemble
  , MalformedNumber(..)
  , MalformedString(..)
  ) where

import Prelude hiding (lines, exp)

import Control.Monad (when)
import Data.Text (Text)
import Language.CCS.Error (internalError, unwrapOrPanic_)
import Language.CCS.Lexer.Morpheme (QuoteType(..), Sign, Radix(..))
import Language.Location (Pos, Span, spanFromPos, mkSpan)
import Language.Nanopass (deflang, defpass)
import Language.Text (SrcText)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Data.Text as T
import qualified Language.CCS.Lexer.NoiseReduction as L0
import qualified Language.Text as Src
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
(CCS from L0:CCS
  (- StrToken)
  (* Token
    (- Number)
    (+ IntegerLiteral Span IntLit)
    (+ FloatingLiteral Span FloLit)

    (- Str)
    (+ StringLiteral Span StrLit)
    (- MlDelim)
    (- MlContent)
    (- MlClose)
    (+ MultilineLiteral Span (* SrcText) SrcText)
  )
)
|]

-- FIXME I can also handle dot/colon sequences here also

data IntLit = IntLit
  { signI :: Sign
  , magI :: Integer -- NOTE should be Natural
  }
  deriving (Eq, Show)

data FloLit
  = FloLit
    { signF :: Sign
    , magF :: Integer -- NOTE should be Natural
    , expF :: (Radix, Integer)
    }
  deriving (Eq, Show)

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

deriving instance Show Token

annotation :: Token -> Span
annotation (Symbol a) = a.span
annotation (Punctuation a _) = a
annotation (Whitespace a) = a.span
annotation (Eol a _) = a
annotation (IntegerLiteral a _) = a
annotation (FloatingLiteral a _) = a
annotation (StringLiteral a _) = a
annotation (MultilineLiteral a _ _) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

xlate :: (MalformedString m, MalformedNumber m) => L0.Token -> m Token
xlate = descendToken Xlate
  { onToken = const Nothing
  , onTokenNumber = number
  , onTokenStr = stdStr
  , onTokenMlDelim = \_ -> internalError "attempt to translate MlDelim token to next lexing stage"
  , onTokenMlContent = \_ -> internalError "attempt to translate MlContent token to next lexing stage"
  , onTokenMlClose = \_ -> internalError "attempt to translate MlClose token to next lexing stage"
  }

------------------
------ Main ------
------------------

assemble ::
  ( MalformedNumber m, MalformedString m )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
assemble inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.MlDelim delim, inp1) -> do
    let st = MlSt
          { mlL = delim.span.start
          , mlLinesReverse = []
          , mlR = delim.span.end
          }
    (mlTok, rest) <- mlMode st inp1
    pure $ do
      yield mlTok
      assemble rest
  Right (other, rest) -> do
    other' <- xlate other
    pure $ do
      yield other'
      assemble rest
  Left r -> pure $ pure r

---------------------
------ Numbers ------
---------------------

number
  :: MalformedNumber m
  => Span
  -> Sign
  -> Radix
  -> Integer
  -> Maybe (Integer, Int)
  -> Maybe Integer
  -> m Token
-- integer literal
number loc sign _ i Nothing Nothing = pure $
  IntegerLiteral loc $ IntLit
    { signI = sign
    , magI = i
    }
-- floating point literal without exponent
number loc sign radix whole (Just (frac, len)) Nothing = do
  when (len == 0) $
    raiseExpectingFractionalDigits loc
  pure $ FloatingLiteral loc $ FloLit
    { signF = sign
    , magF = whole * (radix.base ^ len) + frac
    , expF = (radix, negate $ fromIntegral len)
    }
-- floating point literal with exponent
number loc sign radix whole (Just (frac, len)) (Just exp) = do
  when (len == 0) $
    raiseExpectingFractionalDigits loc
  pure $ FloatingLiteral loc $ FloLit
    { signF = sign
    , magF = whole * (radix.base ^ len) + frac
    , expF = (radix, exp - fromIntegral len)
    }
-- BAD: integer with exponent
number loc sign radix whole Nothing (Just exp) = do
  raiseUnexpectedExponent loc
  pure $ FloatingLiteral loc $ FloLit
    { signF = sign
    , magF = whole
    , expF = (radix, exp)
    }

----------------------------
------ Inline Strings ------
----------------------------

stdStr :: MalformedString m
  => Span
  -> QuoteType
  -> [L0.StrToken]
  -> Maybe QuoteType
  -> m Token
stdStr loc open parts closeM = do
  case closeM of
    Just _ -> pure ()
    Nothing -> raiseExpectingCloseQuote (spanFromPos loc.start)
  let lit = StrLit
        { strTy = mkStrType open closeM
        , strContent = T.concat $ flip map parts $ \case
          L0.StdStr part -> part.text
          L0.StrEscape _ c -> T.singleton c
        }
  pure $ StringLiteral loc lit

mkStrType :: QuoteType -> Maybe QuoteType -> StringType
mkStrType SqlQuote _ = StrLiteral
mkStrType DblQuote (Just Backtick) = OpenTemplate
mkStrType DblQuote _ = StrLiteral
mkStrType Backtick (Just Backtick) = MidTemplate
mkStrType Backtick _ = CloseTemplate
mkStrType (MlQuote _) _ = internalError "mkStrType called on an MlQuote"

-------------------------------
------ Multiline Strings ------
-------------------------------

data MlSt = MlSt
  { mlL :: Pos -- ^ position to the left of the open quotes
  , mlLinesReverse :: [SrcText]
  , mlR :: Pos -- ^ position to the right of the string content so far (or the open quote)
  }

mlMode ::
  ( MalformedString m )
  => MlSt
  -> Stream (Of L0.Token) m r
  -> m (Token, Stream (Of L0.Token) m r)
mlMode st inp0 = S.next inp0 >>= \case
-- OK: accumulate content
  Right (L0.MlContent line, rest) -> do
    let st' = st
          { mlLinesReverse = line : st.mlLinesReverse
          , mlR = line.span.end
          }
    mlMode st' rest
-- OK: ignore line endings
  Right (L0.Eol l _, rest) -> do
    let st' = st{ mlR = l.end }
    mlMode st' rest
-- DONE: ends at close without indent
  Right (L0.MlClose l, rest) -> do
    let tok = MultilineLiteral spn lines lastIndent
        lines = reverse st.mlLinesReverse
        lastIndent = Src.fromPos st.mlR ""
        spn = unwrapOrPanic_ $ mkSpan st.mlL l.end
    pure (tok, rest)
  Right (L0.Whitespace lastIndent, inp1) -> S.next inp1 >>= \case
-- DONE: ends at close with indent
    Right (L0.MlClose l, rest) -> do
      let tok = MultilineLiteral spn lines lastIndent
          spn = unwrapOrPanic_ $ mkSpan st.mlL l.end
          lines = reverse st.mlLinesReverse
      pure (tok, rest)
-- BAD: ends at end of file (with indent)
    Left r -> do
      raiseExpectingCloseQuote $ spanFromPos lastIndent.span.end
      let tok = MultilineLiteral spn lines lastIndent
          lines = reverse st.mlLinesReverse
          spn = unwrapOrPanic_ $ mkSpan st.mlL lastIndent.span.end
      pure (tok, pure r)
-- BAD: internal errors
    Right _ -> internalError "unexpected token before MlClose"
  Right _ -> internalError "unexpected token before MlClose"
-- BAD ends at end of file (without indent)
  Left r -> do
    raiseExpectingCloseQuote $ spanFromPos st.mlR
    let tok = MultilineLiteral spn lines lastIndent
        lines = reverse st.mlLinesReverse
        lastIndent = Src.fromPos st.mlR ""
        spn = unwrapOrPanic_ $ mkSpan st.mlL st.mlR
    pure (tok, pure r)

--------------------
------ Errors ------
--------------------

class Monad m => MalformedNumber m where
  raiseExpectingFractionalDigits :: Span -> m ()
  raiseExpectingExponent :: Span -> m ()
  raiseUnexpectedExponent :: Span -> m ()

class Monad m => MalformedString m where
  raiseExpectingCloseQuote :: Span -> m ()
