{-# LANGUAGE PatternSynonyms #-}

module Language.CCS.Lexer.Sandhi
  ( CCS(..)
  , Token(..)
  , Atom(..)
  , PunctuationType(..)
  , TemplateType(..)
  , BracketType(..)
  , process
  ) where

import Prelude hiding (lines, init)

import Language.CCS.Types.Sandhi

import Control.Monad (when, unless)
import Language.CCS.Util (internalError, unused)
import Language.CCS.Types.Assemble (TemplateType(..))
import Language.CCS.Lexer.Cover (BracketType(..))
import Language.Location (Span, pattern ZwSpan)
import Language.Nanopass (defpass)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))
import Language.CCS.Error (ReaderHooks(..), ReaderStyle(..), ReaderError(..))

import qualified Language.CCS.Types.Indentation as L0
import qualified Streaming as S
import qualified Streaming.Prelude as S

[defpass|(from L0:CCS to CCS)|]

_ignore :: ()
_ignore = unused (descendAtomI, descendPunctuationTypeI)

xlate :: L0.Token -> Token
xlate = descendTokenI XlateI
  { onAtomI = const Nothing
  , onTokenI = \case
    L0.Punctuation spn L0.Dot -> Just $ Atom spn (Symbol ".")
    L0.Punctuation spn L0.Dots2 -> Just $ Atom spn (Symbol "..")
    L0.Punctuation spn L0.Dots3 -> Just $ Atom spn (Symbol "...")
    L0.Punctuation spn L0.Colon -> Just $ Atom spn (Symbol ":")
    L0.Punctuation spn L0.Colons2 -> Just $ Atom spn (Symbol "::")
    L0.Punctuation spn L0.Colons3 -> Just $ Atom spn (Symbol ":::")
    _ -> Nothing
  , onPunctuationTypeI = const Nothing
  , onTokenWhitespaceI = \_ -> internalError "attempt to xlate Whitespace to next lexing stage"
  , onPunctuationTypeDotI = internalError "attempt to xlate Dot to next lexing stage"
  , onPunctuationTypeDots2I = internalError "attempt to xlate Dots2 to next lexing stage"
  , onPunctuationTypeDots3I = internalError "attempt to xlate Dots3 to next lexing stage"
  , onPunctuationTypeColonI = internalError "attempt to xlate Colon to next lexing stage"
  , onPunctuationTypeColons2I = internalError "attempt to xlate Colons2 to next lexing stage"
  , onPunctuationTypeColons3I = internalError "attempt to xlate Colons3 to next lexing stage"
  , onPunctuationTypeBackslashI = ContinueLine
  }

------------------
------ Main ------
------------------

process :: (ReaderHooks m)
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
process = mapWithLookaround $ \(prev, here, next) -> case here of
-- most tokens are just atoms, and we pass on to atomSandhi
  L0.Atom _ _ -> atomSandhi prev here next
  L0.StringTemplate _ ty _ -> case ty of
    -- we only check atomSandi on the open templates b/c we raise CrammedTokens only when looking backwards
    OpenTemplate -> atomSandhi prev here next
    _ -> pure [xlate here]
  L0.Punctuation spn ty -> case ty of
    L0.Dots2 -> atomSandhi prev here next
    L0.Dots3 -> atomSandhi prev here next
    L0.Colons2 -> atomSandhi prev here next
    L0.Colons3 -> atomSandhi prev here next
-- punctuation creates interesting effects though!
    L0.Open _ -> do
      let addChain = if isAtom ViewFromRight prev || isClose prev
            then (Punctuation (ZwSpan spn.start) Chain :)
            else id
      pure $ addChain [xlate here]
    L0.Close _ -> pure [xlate here]
    L0.Dot ->
      if | isAtom ViewFromRight prev || isClose prev
         , isAtom ViewFromLeft next
          -> pure [Punctuation spn Chain]
         | isSpace prev
         , isSpace next
          -> pure [xlate here]
         | otherwise -> do
          recoverableError $ UnexpectedDot here.span prev next
          pure []
    L0.Colon ->
      if | isIndent next -> pure [Punctuation spn StartBlock]
         | isSpace prev && isSpace next -> pure [Atom spn $ Symbol "."]
         | isSpace next -> pure [Punctuation spn Pair]
         | isSymbol prev && isAtom ViewFromLeft next -> pure [Punctuation spn Qualify]
         | otherwise -> do
          recoverableError $ UnexpectedColon here.span prev next
          pure []
    L0.Comma -> separatorSandhi prev (spn, Comma) next
    L0.Semicolon -> separatorSandhi prev (spn, Semicolon) next
    L0.Backslash -> do
      case next of
        Just (L0.Indent _) -> pure ()
        _ -> recoverableError $ UnexpectedBackslash here.span next
      pure [xlate here]
-- and then we just get boring whitespace tokens
  L0.Whitespace _ -> pure []
  L0.Indent _ -> do
    unless (canStartIndent prev) $ do
      recoverableError $ UnexpectedIndent here.span prev
    pure [xlate here]
  L0.Nextline _ -> pure [xlate here]
  L0.Dedent _ -> pure [xlate here]

type Process m a = Maybe L0.Token -> a -> Maybe L0.Token -> m [Token]

atomSandhi :: ReaderHooks m => Process m L0.Token
atomSandhi (Just prev) atom _ = do
  when (isAtom ViewFromRight (Just prev) || isClose (Just prev)) $ do
    recoverableError $ CrammedTokens (ZwSpan atom.span.start) prev atom
  pure [xlate atom]
atomSandhi Nothing atom _ = pure [xlate atom]

separatorSandhi :: ReaderHooks m => Process m (Span, PunctuationType)
separatorSandhi _ (spn, ty) next_m = do
  let sep = Punctuation spn ty
  case next_m of
    Just next | not $ isSpace next_m -> do
      styleNote $ ExpectingWhitespaceAfterSeparator spn sep next
    _ -> pure ()
  pure [sep]

------ Recursion Relation ------

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

---------------------
------ Helpers ------
---------------------

data ViewFrom = ViewFromLeft | ViewFromRight
  deriving (Eq)

isAtom :: ViewFrom -> Maybe L0.Token -> Bool
isAtom _ Nothing = False
isAtom view (Just tok) = case tok of
  L0.Atom _ _ -> True
  L0.StringTemplate _ ty _ -> case ty of
    OpenTemplate -> view == ViewFromLeft -- template from the inside
    MidTemplate -> False
    CloseTemplate -> view == ViewFromRight -- template froom the outside
  L0.Punctuation _ punct -> case punct of
    L0.Dots2 -> True
    L0.Dots3 -> True
    L0.Colons2 -> True
    L0.Colons3 -> True
    _ -> False
  L0.Whitespace _ -> False
  L0.Indent _ -> False
  L0.Nextline _ -> False
  L0.Dedent _ -> False

isSymbol :: Maybe L0.Token -> Bool
isSymbol (Just (L0.Atom _ (L0.Symbol _))) = True
isSymbol _ = False

isClose :: Maybe L0.Token -> Bool
isClose (Just (L0.Punctuation _ (L0.Close _))) = True
isClose _ = False

isIndent :: Maybe L0.Token -> Bool
isIndent (Just (L0.Indent _)) = True
isIndent _ = False

isSpace :: Maybe L0.Token -> Bool
isSpace Nothing = True
isSpace (Just tok) = case tok of
  L0.Whitespace _ -> True
  L0.Indent _ -> True
  L0.Nextline _ -> True
  L0.Dedent _ -> True
  _ -> False

canStartIndent :: Maybe L0.Token -> Bool
canStartIndent Nothing = False
canStartIndent (Just tok) = case tok of
  L0.Punctuation _ L0.Colon -> True
  L0.Punctuation _ (L0.Open _) -> True
  L0.Punctuation _ L0.Backslash -> True
  _ -> False
