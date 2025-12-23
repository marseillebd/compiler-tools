module Language.CCS.Lexer.Sandhi
  ( CCS(..)
  , Token(..)
  , PunctuationType(..)
  , process
  , SandhiError(..)
  ) where

import Prelude hiding (lines, init)

import Control.Monad (when, unless)
import GHC.Records (HasField(..))
import Language.CCS.Error (internalError)
import Language.CCS.Lexer.Assemble (TemplateType(..))
import Language.Location (Span, spanFromPos)
import Language.Nanopass (deflang, defpass)
import Streaming.Prelude (yield)
import Streaming (Stream, Of(..))

import qualified Language.CCS.Lexer.Indentation as L0
import qualified Streaming as S
import qualified Streaming.Prelude as S

[deflang|
(CCS from L0:CCS
  (* Token
    (- Whitespace)
  )
  (* PunctuationType
    (- Dot) (- Dots2) (- Dots3)
    (+ StartBlock)
    (+ Chain)
    (- Colon) (- Colons2) (- Colons3)
    (+ Pair) (+ Qualify)
    (- Backslash)
    (+ ContinueLine)
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
  getField (Indent a) = a
  getField (Nextline a) = a
  getField (Dedent a) = a

$(pure [])

[defpass|(from L0:CCS to CCS)|]

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
  , onPunctuationTypeDotI = Open $ internalError "attempt to xlate Dot to next lexing stage"
  , onPunctuationTypeDots2I = Open $ internalError "attempt to xlate Dots2 to next lexing stage"
  , onPunctuationTypeDots3I = Open $ internalError "attempt to xlate Dots3 to next lexing stage"
  , onPunctuationTypeColonI = Open $ internalError "attempt to xlate Colon to next lexing stage"
  , onPunctuationTypeColons2I = Open $ internalError "attempt to xlate Colons2 to next lexing stage"
  , onPunctuationTypeColons3I = Open $ internalError "attempt to xlate Colons3 to next lexing stage"
  , onPunctuationTypeBackslashI = ContinueLine
  }

------------------
------ Main ------
------------------

process :: (SandhiError m)
  => Stream (Of L0.Token) m r
  -> Stream (Of Token) m r
process = mapWithLookaround $ \(prev, here, next) -> case here of
-- most tokens are just atoms, and we pass on to atomSandhi
  L0.Atom _ _ -> atomSandhi prev here next
  L0.StringTemplate _ ty _ -> case ty of
    -- we only check atomSandi on the open templates b/c we raiseCrammedTokens only when looking backwards
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
            then (Punctuation (spanFromPos spn.start) Chain :)
            else id
      pure $ addChain [xlate here]
    L0.Close _ -> pure [xlate here]
    L0.Dot ->
      if | isAtom ViewFromRight prev && isClose prev, isAtom ViewFromLeft next
          -> pure [Punctuation spn Chain]
         | isSpace prev && isSpace next
          -> pure [xlate here]
         | otherwise -> do
          raiseUnexpectedDot here.span
          pure []
    L0.Colon ->
      if | isIndent next -> pure [Punctuation spn StartBlock]
         | isSpace prev && isSpace next -> pure [Atom spn $ Symbol "."]
         | isSpace next -> pure [Punctuation spn Pair]
         | isSymbol prev && isAtom ViewFromLeft next -> pure [Punctuation spn Qualify]
         | otherwise -> do
          raiseUnexpectedColon spn
          pure []
    L0.Comma -> separatorSandhi prev (spn, Comma) next
    L0.Semicolon -> separatorSandhi prev (spn, Semicolon) next
    L0.Backslash -> do
      case next of
        Just (L0.Indent _) -> pure ()
        _ -> raiseUnexpectedBackslash here.span
      pure [xlate here]
-- and then we just get boring whitespace tokens
  L0.Whitespace _ -> pure []
  L0.Indent _ -> do
    unless (canStartIndent prev) $ do
      raiseUnexpectedIndent here.span
    pure [xlate here]
  L0.Nextline _ -> pure [xlate here]
  L0.Dedent _ -> pure [xlate here]

type Process m a = Maybe L0.Token -> a -> Maybe L0.Token -> m [Token]

atomSandhi :: SandhiError m => Process m L0.Token
atomSandhi prev atom _ = do
  when (isAtom ViewFromRight prev || isClose prev) $ do
    raiseCrammedTokens (spanFromPos atom.span.start)
  pure [xlate atom]

separatorSandhi :: SandhiError m => Process m (Span, PunctuationType)
separatorSandhi _ (spn, ty) next = do
  unless (isSpace next) $ do
    raiseExpectedWhitespace (spanFromPos spn.end)
  pure [Punctuation spn ty]

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

--------------------
------ Errors ------
--------------------

class Monad m => SandhiError m where
  raiseCrammedTokens :: Span -> m ()
  raiseExpectedWhitespace :: Span -> m ()
  raiseUnexpectedIndent :: Span -> m ()
  raiseUnexpectedWhitespace :: Span -> m ()
  raiseUnexpectedDot :: Span -> m ()
  raiseUnexpectedColon :: Span -> m ()
  raiseUnexpectedBackslash :: Span -> m ()
