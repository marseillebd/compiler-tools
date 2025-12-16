module Language.CCS.Lexer.NoiseReduction
  ( CCS(..)
  , Token(..)
  , annotation
  , pipeline
  , DeleteComment(..)
  , RaiseIllegalBytes(..)
  , WhitespaceError(..)
  ) where

import Control.Monad (when)
import Data.Function ((&))
import Data.Text (Text)
import Language.CCS.Error (internalError)
import Language.Location (Span, spanFromPos)
import Language.Nanopass (deflang, defpass)
import Streaming (Stream, Of(..))
import Streaming.Prelude (yield)
import Language.CCS.Lexer.Morpheme (EolType(..))

import qualified Language.CCS.Lexer.Morpheme as L0
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
((CCS loc) from L0:CCS
  (* Token
    (- Comment)
    (- Illegal)
    (- Eol)
    (+ Indentation loc Text)
  )
)
|]

annotation :: Token a -> a
annotation (Symbol a _) = a
annotation (Sign a _) = a
annotation (Radix a _) = a
annotation (Digits a _ _) = a
annotation (Power a) = a
annotation (Punctuation a _) = a
annotation (Quote a _) = a
annotation (StdStr a _) = a
annotation (StrEscape a _) = a
annotation (Indentation a _) = a
annotation (Whitespace a _) = a

$(pure [])
[defpass|(from L0:CCS to CCS)|]

xlate :: L0.Token loc -> Token loc
xlate = descendTokenI XlateI
  { onTokenI = const Nothing
  , onTokenCommentI = \_ _ -> internalError "attempt to translate Comment token to next lexing stage"
  , onTokenEolI = \_ _ -> internalError "attempt to translate Eol token to next lexing stage"
  , onTokenIllegalI = \_ _ -> internalError "attempt to translate Illegal token to next lexing stage"
  }

-- | Gets rid of comments, then trailing whitespace.
-- Raises an error on illegal tokens.
-- It also checks that the file has consistent newline sequences, and that it ends in a newline.
-- The caller gets the chance to do something with comments before their removal.
-- Likewise, the caller gets to choose whether to abort or do error recovery on illegal tokens.
pipeline ::
  ( Monad m
  , DeleteComment m
  , RaiseIllegalBytes m
  , WhitespaceError m )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
pipeline input
  = input
  & consistentNewlines Nothing
  & endsInNewline
  & simpleDeletions
  & untrailing

simpleDeletions ::
  ( Monad m
  , DeleteComment m
  , RaiseIllegalBytes m )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (Token Span)) m r
simpleDeletions = startLine
  where
  startLine inp0 = S.effect $ S.next inp0 >>= \case
  -- whitespace at start of line becomes indentation
    Right (L0.Whitespace l txt, rest) -> pure $ do
      yield (Indentation l txt)
      loop rest
  -- otherwise, we create empty indentation
    Right (other, inp1) -> pure $ do
      let l = spanFromPos (L0.annotation other).start
          rest = yield other >> inp1
      yield (Indentation l "")
      loop rest
    Left r -> pure $ pure r
  loop inp0 = S.effect $ S.next inp0 >>= \case
  -- raise error and delete illegal tokens
    Right (L0.Illegal l txt, inp1) -> do
      raiseIllegalBytesOrChars l txt
      pure $ loop inp1
  -- delete comment tokens
    Right (L0.Comment l txt, inp1) -> do
      deleteComment l txt
      pure $ loop inp1
  -- eol usually goes back to `startLine` processing
    Right (L0.Eol l _, inp1) -> S.next inp1 >>= \case
      Right (other, inp2) -> pure $ do
        let rest = yield other >> inp2
        startLine rest
  -- but if we're at the end of the file, we want to generate a final indentation
      Left r -> pure $ do
        yield (Indentation (spanFromPos l.end) "")
        pure r
  -- base cases
    Right (other, rest) -> pure $ do
      yield (xlate other)
      loop rest
    Left r -> pure $ pure r

untrailing ::
  ( Monad m
  , WhitespaceError m )
  => Stream (Of (Token Span)) m r
  -> Stream (Of (Token Span)) m r
untrailing inp0 = S.effect $ S.next inp0 >>= \case
  Left r -> pure $ pure r
  Right (lws@(Whitespace l _), inp1) -> S.next inp1 >>= \case
-- remove whitespace before end of file
    Left r -> do
      raiseTrailingWhitespace l
      pure $ pure r
-- remove whitespace before end of line
    Right (nl@(Indentation _ _), rest) -> do
      raiseTrailingWhitespace l
      pure $ yield nl >> untrailing rest
-- leave non-trailing whitespace alone
    Right (other, inp2) -> do
      let rest = yield other >> inp2
      pure $ yield lws >> untrailing rest
-- leave non-whitespace tokens alone
  Right (other, inp1) ->
    pure $ yield other >> untrailing inp1

consistentNewlines ::
  ( Monad m
  , WhitespaceError m )
  => Maybe (Span, EolType)
  -> Stream (Of (L0.Token Span)) m r
  -> Stream (Of (L0.Token Span)) m r
consistentNewlines Nothing = loop
  where
  loop inp0 = S.effect $ S.next inp0 >>= \case
    Left r -> pure $ pure r
    Right (nl@(L0.Eol l ty), rest) ->
      pure $ yield nl >> consistentNewlines (Just (l, ty)) rest
    Right (other, rest) -> pure $ yield other >> loop rest
consistentNewlines (Just (l, ty)) = loop
  where
  loop inp0 = S.effect $ S.next inp0 >>= \case
    Left r -> pure $ pure r
    Right (nl@(L0.Eol l' ty'), rest) -> do
      when (ty /= ty') $ raiseInconsistentNewlines InconsistentNewlines
        { expected = (l, ty)
        , found = (l', ty')
        }
      pure $ yield nl >> loop rest
    Right (other, rest) -> pure $ yield other >> loop rest

endsInNewline ::
  ( Monad m
  , WhitespaceError m )
  => Stream (Of (L0.Token Span)) m r
  -> Stream (Of (L0.Token Span)) m r
endsInNewline inp0 = S.effect $ S.next inp0 >>= \case
-- at last token
  Right (x, inp1) -> S.next inp1 >>= \case
    Left r -> do
      case x of
        L0.Eol l Eof -> raiseNoNlAtEof l
        _ -> pure ()
      pure $ yield x >> pure r
-- not at last token
    Right (y, inp2) -> do
      let rest = yield y >> inp2
      pure $ yield x >> endsInNewline rest
  Left r -> pure $ pure r

class DeleteComment m where
  -- | For most purposes, we'd just strip comments out of the token stream with no fanfare.
  -- Indeed, a valid implementation just ignores the comment Span and 'Text' and returns the unit.
  --
  -- However, we might have a processor that would like to
  -- - search comments for tags like todo or debug
  -- - lint the comment text for style (like looking for typos, or ensuring there's a space after the hash)
  -- - keep the comment around for later, when perhaps it attaches to some nearby bit of code
  --   (this is often used for documentation, but I don't recommend it personally.
  --   I think comments should be there for the person who is reading the code, and no one and nothing else.)
  deleteComment :: Span -> Text -> m ()

-- Instead of directly erroring out, allow the caller to decide:
-- - should we recover from the error and continue? (my morphemes, we already have)
-- - should I merge adjacent illegal bytes?
-- - how should the errors be reported?
class RaiseIllegalBytes m where
  -- TODO someday, I'll distinguish between decoding errors and illegal codepoints
  -- also bad bytes are just being replaced by the unicode Replacement Character
  raiseIllegalBytesOrChars :: Span -> Text -> m ()

data InconsistentNewlines = InconsistentNewlines
  { expected :: (Span, EolType)
  , found :: (Span, EolType)
  }
  deriving (Show)
class WhitespaceError m where
  raiseTrailingWhitespace :: Span -> m ()
  raiseInconsistentNewlines :: InconsistentNewlines -> m ()
  raiseNoNlAtEof :: Span -> m ()
