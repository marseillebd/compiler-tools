module Language.CCS.Lexer.Morpheme.Assemble.Numbers
  ( CCS(..)
  , Token(..)
  , annotation
  , IntLit(..)
  , FloLit(..)
  , assemble
  , MalformedNumber(..)
  ) where

import Prelude hiding (exp)

import Control.Monad (when)
import Language.CCS.Error (internalError, unwrapOrPanic_)
import Language.Location (Pos, Span, spanFromPos, mkSpan)
import Language.Nanopass (deflang, defpass)
import Streaming (Stream, Of(..))
import Streaming.Prelude (yield)
import Language.CCS.Lexer.Morpheme (Sign(..), Radix(..), PunctuationType(Dot))

import qualified Language.CCS.Lexer.Morpheme.NoiseReduction as L0
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
((CCS loc) from L0:CCS
  (* Token
    (- Sign)
    (- Radix)
    (- Digits)
    (- Power)
    (+ IntegerLiteral loc IntLit)
    (+ FloatingLiteral loc FloLit)
  )
)
|]

deriving instance Show a => Show (Token a)
deriving instance Functor Token

annotation :: Token a -> a
annotation (Symbol a _) = a
annotation (Punctuation a _) = a
annotation (Quote a _) = a
annotation (StdStr a _) = a
annotation (StrEscape a _) = a
annotation (Whitespace a _) = a
annotation (Indentation a _) = a
annotation (IntegerLiteral a _) = a
annotation (FloatingLiteral a _) = a

data IntLit = IntLit
  { signI :: Sign
  , magI :: Integer -- NOTE should be Natural
  }
  deriving (Eq, Show)

data FloLit
  = FloLit
    { signF :: Sign
    , magF :: Integer -- NOTE should be Natural
    , expF :: (Radix, Int)
    }
  deriving (Eq, Show)

$(pure [])

[defpass|(from L0:CCS to CCS)|]

xlate :: L0.Token loc -> Token loc
xlate = descendTokenI XlateI
  { onTokenI = const Nothing
  , onTokenSignI = \_ _ -> internalError "attempt to translate Sign token to next lexing stage"
  , onTokenRadixI = \_ _ -> internalError "attempt to translate Radix token to next lexing stage"
  , onTokenDigitsI = \_ _ _ -> internalError "attempt to translate Digits token to next lexing stage"
  , onTokenPowerI = \_ -> internalError "attempt to translate Power token to next lexing stage"
  }

assemble ::
  ( Monad m
  , MalformedNumber m
  )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
assemble inp0 = S.effect $ S.next inp0 >>= \case
  Right (L0.Sign lSign sign, inp1) -> S.next inp1 >>= \case
    Right (L0.Radix lRadix radix, inp2) -> S.next inp2 >>= \case
-- OK: sign, radix, digits
      Right (L0.Digits lDigits i _, rest) -> do
        pure $ assemble2 (lSign.start, sign, radix, i, lDigits.end) rest
-- NOTE enhanced error recovery if it's sign, radix, dot, digits: uze zero whole part
-- NOTE enhanced error recovery if it's sign, radix, power, digits: uze zero whole part
-- BAD: sign, radix, no digits
      Right (other, rest) -> do
        raiseExpectingIntegerDigits (L0.annotation other)
        pure $ assemble rest
      Left r -> do
        raiseExpectingIntegerDigits (spanFromPos lRadix.end)
        pure $ pure r
    Right (L0.Digits lDigits i _, rest) -> do
-- OK: sign, digits
      pure $ assemble2 (lSign.start, sign, Base10, i, lDigits.end) rest
-- NOTE enhanced error recovery if it's sign, dot, digits: uze zero whole part
-- NOTE enhanced error recovery if it's sign, power, digits: uze zero whole part
-- BAD: sign, no digits
    Right (other, rest) -> do
      raiseExpectingIntegerDigits (L0.annotation other)
      pure $ assemble rest
    Left r -> do
      raiseUnexpectedSign lSign
      pure $ pure r
  Right (L0.Radix lRadix radix, inp1) -> S.next inp1 >>= \case
-- OK: radix, digits
    Right (L0.Digits lDigits i _, rest) -> do
      pure $ assemble2 (lRadix.start, Positive, radix, i, lDigits.end) rest
-- NOTE enhanced error recovery if it's radix, dot, digits: uze zero whole part
-- NOTE enhanced error recovery if it's radix, power, digits: uze zero whole part
-- BAD: radix, no digits
    Right (other, rest) -> do
      raiseExpectingIntegerDigits (L0.annotation other)
      pure $ assemble rest
    Left r -> do
      raiseExpectingIntegerDigits (spanFromPos lRadix.end)
      pure $ pure r
-- OK: digits only
  Right (L0.Digits lDigits i _, rest) -> do
    pure $ assemble2 (lDigits.start, Positive, Base10, i, lDigits.end) rest
-- BAD: power sign only
  Right (L0.Power lPower, rest) -> do
    raiseUnexpectedPower lPower
    pure $ assemble rest
-- OK: base cases
  Right (other, rest) -> pure $ yield (xlate other) >> assemble rest
  Left r -> pure $ pure r

assemble2 ::
  ( Monad m
  , MalformedNumber m
  )
  => (Pos, Sign, Radix, Integer, Pos)
  -> Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
assemble2 (start, sign, radix, whole, end) inp0 = S.effect $ S.next inp0 >>= \case
------ Fractional with Exponent ------
  Right (dot@(L0.Punctuation _ Dot), inp1) -> S.next inp1 >>= \case
    Right (L0.Digits lDigits frac len, inp2) -> S.next inp2 >>= \case
      Right (L0.Power lPower, inp3) -> S.next inp3 >>= \case
        Right (L0.Sign lSign expSign, inp4) -> S.next inp4 >>= \case
-- OK: dot, mantissa, power char, sign, exponent
          Right (L0.Digits lExp expMag _, rest) -> pure $ do
            let lit = mkFlo (sign, whole, (radix, frac, len), Just (expSign, expMag))
                spn = unwrapOrPanic_ $ mkSpan start lExp.end
            yield (FloatingLiteral spn lit)
            assemble rest
-- BAD: dot, mantissa, power char, sign, but NO exponent
          Right (other, rest) -> do
            raiseExpectingExponent (L0.annotation other)
            pure $ assemble $ yield other >> rest
          Left r -> do
            raiseExpectingExponent lSign
            pure $ pure r
-- OK: dot, mantissa, power char, (positive) exponent
        Right (L0.Digits lExp expMag _, rest) -> pure $ do
          let lit = mkFlo (sign, whole, (radix, frac, len), Just (Positive, expMag))
              spn = unwrapOrPanic_ $ mkSpan start lExp.end
          yield (FloatingLiteral spn lit)
          assemble rest
-- BAD: dot, mantissa, power char, but NO exponent
        Right (other, rest) -> do
          raiseExpectingExponent (L0.annotation other)
          pure $ assemble $ yield other >> rest
        Left r -> do
          raiseExpectingExponent (spanFromPos lPower.end)
          pure $ pure r
-- NOTE better error recovery on dot+power char: treat mantissa as zero
------ Fractional without Exponent ------
-- OK: dot, mantissa, NO exponent sequence
      Right (other, rest) -> pure $ do
        let lit = mkFlo (sign, whole, (radix, frac, len), Nothing)
            spn = unwrapOrPanic_ $ mkSpan start lDigits.end
        yield (FloatingLiteral spn lit)
        assemble $ yield other >> rest
      Left r -> pure $ do
        let lit = mkFlo (sign, whole, (radix, frac, len), Nothing)
            spn = unwrapOrPanic_ $ mkSpan start lDigits.end
        yield (FloatingLiteral spn lit)
        pure r
-- BAD: dot, NO mantissa
    Right (other, rest) -> pure $ do
      let lit = mkInt (sign, whole, Nothing)
          spn = unwrapOrPanic_ $ mkSpan start end
      yield (IntegerLiteral spn lit)
      assemble $ yield dot >> yield other >> rest
    Left r -> pure $ do
      let lit = mkInt (sign, whole, Nothing)
          spn = unwrapOrPanic_ $ mkSpan start end
      yield (IntegerLiteral spn lit)
      assemble $ yield dot >> pure r
------ Integer with (positive) Exponent ------
  Right (L0.Power lPower, inp3) -> S.next inp3 >>= \case
    Right (L0.Sign lSign expSign, inp4) -> S.next inp4 >>= \case
-- OK: power char, sign, exponent
      Right (L0.Digits lExp expMag _, rest) -> do
        when (expSign == Negative) $
          raiseNegativeExponentForInteger lSign
        pure $ do
          let lit = mkInt (sign, whole, Just (radix, expMag))
              spn = unwrapOrPanic_ $ mkSpan start lExp.end
          yield (IntegerLiteral spn lit)
          assemble rest
-- BAD: power char, sign, NO exponent
      Right (other, rest) -> do
        raiseExpectingExponent (L0.annotation other)
        pure $ assemble $ yield other >> rest
      Left r -> do
        raiseExpectingExponent lSign
        pure $ pure r
-- OK: power char, positive exponent
    Right (L0.Digits lExp expMag _, rest) -> pure $ do
      let lit = mkInt (sign, whole, Just (radix, expMag))
          spn = unwrapOrPanic_ $ mkSpan start lExp.end
      yield (IntegerLiteral spn lit)
      assemble rest
-- BAD: power char, but NO exponent
    Right (other, rest) -> do
      raiseExpectingExponent (L0.annotation other)
      pure $ assemble $ yield other >> rest
    Left r -> do
      raiseExpectingExponent (spanFromPos lPower.end)
      pure $ pure r
------ Plain Integer ------
-- OK: no mantissa or power
  Right (other, rest) -> do
    let lit = mkInt (sign, whole, Nothing)
        spn = unwrapOrPanic_ $ mkSpan start end
    pure $ do
      yield (IntegerLiteral spn lit)
      assemble (yield other >> rest)
  Left r -> do
    let lit = mkInt (sign, whole, Nothing)
        spn = unwrapOrPanic_ $ mkSpan start end
    pure $ do
      yield (IntegerLiteral spn lit)
      pure r -- DEBUG

mkInt :: (Sign, Integer, Maybe (Radix, Integer)) -> IntLit
mkInt (sign, whole, Nothing) = IntLit
  { signI = sign
  , magI = whole
  }
mkInt (sign, whole, Just (radix, eMag)) = IntLit
  { signI = sign
  , magI = whole * (radix.base ^ eMag)
  }

mkFlo :: (Sign, Integer, (Radix, Integer, Int), Maybe (Sign, Integer)) -> FloLit
mkFlo (sign, whole, (radix, frac, len), Nothing) = FloLit
  { signF = sign
  , magF = whole * (radix.base ^ len) + frac
  , expF = (radix, negate len)
  }
mkFlo (sign, whole, (radix, frac, len), Just (expSign, expMag)) =
  let exp = if expSign == Positive then expMag else negate expMag
   in FloLit
      { signF = sign
      , magF = whole * (radix.base ^ len) + frac
      , expF = (radix, fromInteger exp - len)
      }

class MalformedNumber m where
  raiseExpectingIntegerDigits :: Span -> m ()
  raiseNegativeExponentForInteger :: Span -> m ()
  raiseExpectingExponent :: Span -> m ()
  raiseUnexpectedSign :: Span -> m ()
  raiseUnexpectedPower :: Span -> m ()
