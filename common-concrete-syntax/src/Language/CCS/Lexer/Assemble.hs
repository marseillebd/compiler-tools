module Language.CCS.Lexer.Assemble
  ( CCS(..)
  , Token(..)
  , StrLit(..)
  , StringType(..)
  , PunctuationType(..)
  , assemble
  , MalformedPunctuation(..)
  , MalformedNumber(..)
  , MalformedString(..)
  ) where

import Prelude hiding (lines, exp)

import Control.Monad (when)
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.CCS.Error (internalError)
import Language.CCS.Lexer.Morpheme (QuoteType(..), Sign, Radix(..))
import Language.Location (Span, spanFromPos)
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

  (- StrToken)

  (* PunctuationType
    (- Dots)
    (+ Dot) (+ Dots2) (+ Dots3)
    (- Colons)
    (+ Colon) (+ Colons2) (+ Colons3)
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
deriving instance Show PunctuationType

instance HasField "span" Token Span where
  getField (Symbol a) = a.span
  getField (Punctuation a _) = a
  getField (Whitespace a) = a.span
  getField (Eol a _) = a
  getField (IntegerLiteral a _) = a
  getField (FloatingLiteral a _) = a
  getField (StringLiteral a _) = a
  getField (MultilineLiteral a _ _) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

xlate :: (MalformedPunctuation m, MalformedNumber m, MalformedString m)
  => L0.Token -> m Token
xlate = descendToken Xlate
  { onToken = \case
    L0.Punctuation loc (L0.Dots n) -> Just $ longDots loc n
    L0.Punctuation loc (L0.Colons n) -> Just $ longColons loc n
    _ -> Nothing
  , onTokenStr = stdStr
  , onPunctuationType = const Nothing
  , onTokenNumber = number
  , onTokenMlDelim = \_ -> internalError "attempt to translate MlDelim token to next lexing stage"
  , onTokenMlContent = \_ -> internalError "attempt to translate MlContent token to next lexing stage"
  , onTokenMlClose = \_ -> internalError "attempt to translate MlClose token to next lexing stage"
  , onPunctuationTypeDots = \_ -> internalError "attempt to translate Dots token to next lexing stage"
  , onPunctuationTypeColons = \_ -> internalError "attempt to translate Colons token to next lexing stage"
  }

------------------
------ Main ------
------------------

assemble ::
  ( MalformedPunctuation m, MalformedNumber m, MalformedString m )
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
assemble inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.MlDelim delim, inp1) -> do
    let st = MlSt
          { mlSpan = delim.span
          , mlLinesReverse = []
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

-------------------------
------ Punctuation ------
-------------------------

longDots
  :: MalformedPunctuation m
  => Span
  -> Int
  -> m Token
longDots loc 1 = pure $ Punctuation loc Dot
longDots loc 2 = pure $ Punctuation loc Dots2
longDots loc 3 = pure $ Punctuation loc Dots3
longDots loc _ = do
  raiseTooManyDots loc
  pure $ Punctuation loc Dots3

longColons
  :: MalformedPunctuation m
  => Span
  -> Int
  -> m Token
longColons loc 1 = pure $ Punctuation loc Colon
longColons loc 2 = pure $ Punctuation loc Colons2
longColons loc 3 = pure $ Punctuation loc Colons3
longColons loc _ = do
  raiseTooManyColons loc
  pure $ Punctuation loc Colons3

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
  { mlSpan :: Span
  , mlLinesReverse :: [SrcText]
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
          , mlSpan = st.mlSpan <> line.span
          }
    mlMode st' rest
-- OK: ignore line endings
  Right (L0.Eol l _, rest) -> do
    let st' = st{ mlSpan = st.mlSpan <> l }
    mlMode st' rest
-- DONE: ends at close without indent
  Right (L0.MlClose l, rest) -> do
    let tok = MultilineLiteral spn lines lastIndent
        lines = reverse st.mlLinesReverse
        lastIndent = Src.fromPos st.mlSpan.end ""
        spn = st.mlSpan <> l
    pure (tok, rest)
  Right (L0.Whitespace lastIndent, inp1) -> S.next inp1 >>= \case
-- DONE: ends at close with indent
    Right (L0.MlClose l, rest) -> do
      let tok = MultilineLiteral spn lines lastIndent
          spn = st.mlSpan <> l
          lines = reverse st.mlLinesReverse
      pure (tok, rest)
-- BAD: ends at end of file (with indent)
    Left r -> do
      raiseExpectingCloseQuote $ spanFromPos lastIndent.span.end
      let tok = MultilineLiteral spn lines lastIndent
          lines = reverse st.mlLinesReverse
          spn = st.mlSpan <> lastIndent.span
      pure (tok, pure r)
-- BAD: internal errors
    Right _ -> internalError "unexpected token before MlClose"
  Right _ -> internalError "unexpected token before MlClose"
-- BAD ends at end of file (without indent)
  Left r -> do
    raiseExpectingCloseQuote $ spanFromPos st.mlSpan.end
    let tok = MultilineLiteral st.mlSpan lines lastIndent
        lines = reverse st.mlLinesReverse
        lastIndent = Src.fromPos st.mlSpan.end ""
    pure (tok, pure r)

--------------------
------ Errors ------
--------------------

class Monad m => MalformedPunctuation m where
  raiseTooManyDots :: Span -> m ()
  raiseTooManyColons :: Span -> m ()

class Monad m => MalformedNumber m where
  raiseExpectingFractionalDigits :: Span -> m ()
  raiseExpectingExponent :: Span -> m ()
  raiseUnexpectedExponent :: Span -> m ()

class Monad m => MalformedString m where
  raiseExpectingCloseQuote :: Span -> m ()
