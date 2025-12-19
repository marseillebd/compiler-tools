module Language.CCS.Lexer.Sandhi
  ( CCS(..)
  , Token(..)
  , PunctuationType(..)
  ) where

import Prelude hiding (lines, init)

import Control.Applicative ((<|>))
import Control.Monad (forM, when, unless, replicateM_, void)
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.CCS.Error (internalError, unwrapOrPanic_)
import Language.Location (incCol, Span, mkSpan, spanFromPos)
import Language.Nanopass (deflang, defpass)
import Language.Text (SrcText)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Data.Text as T
import qualified Language.CCS.Lexer.Indentation as L0
import qualified Language.Text as Src
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
(CCS from L0:CCS
  (* PunctuationType
    (- Dots2) (- Dots3)
    (- Colons2) (- Colons3)
  )
)
|]

deriving instance Show Token
deriving instance Show PunctuationType

instance HasField "span" Token Span where
  getField (Symbol a) = a.span
  getField (Punctuation a _) = a
  getField (Whitespace a) = a.span
  getField (Indent a) = a
  getField (Nextline a) = a
  getField (Dedent a) = a
  getField (IntegerLiteral a _) = a
  getField (FloatingLiteral a _) = a
  getField (StringLiteral a _) = a
  getField (MultilineLiteral a _) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

------------------
------ Main ------
------------------

process :: (Monad m)
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
process = mapWithLookaround $ \case
  (prev, L0.Symbol sym, next) -> symbolSandhi prev sym next
  -- FIXME

------ Recursion Relation ------

type Process m a = Maybe L0.Token -> a -> Maybe L0.Token -> m [Token]

-- | The idea is to read in a token and process it using:
-- a) the saved last token, and b) lookahead to the next token.
mapWithLookaround
  :: (Monad m)
  => ((Maybe L0.Token, L0.Token, Maybe L0.Token) -> m [Token])
  -> Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
mapWithLookaround f = loop Nothing
  where
  loop prev inp0 = S.effect $ S.next inp0 >>= \case
    Right (here, inp1) -> S.next inp1 >>= \case
      Right (next, inp2) -> do
        here' <- f (prev, here, Just next)
        let rest = yield next >> inp2
        pure $ do
          mapM_ yield here'
          loop (Just here) rest
      Left r -> do
        here' <- f (prev, here, Nothing)
        pure $ do
          mapM_ yield here'
          pure r
    Left r -> pure $ pure r

-------------------------
------ Translators ------
-------------------------

symbolSandhi :: Monad m => Process m SrcText
symbolSandhi = undefined -- FIXME
