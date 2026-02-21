-- TODO I may end up changing the name to ingest, or validate, or something
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Language.CCS.Recognize
  ( Recog
  , runRecog
  , parse, pop
  , fail, report
  , test
  -- , raiseR, reportR
  -- , catchR, labelR, explainR

  , Result(..)
  , ErrorTree(..)

  , headM
  , leftM, onLeftM
  , optional
  ) where

import Prelude hiding (fail)

import Control.Monad (ap)
import Data.Foldable (Foldable(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.CCS.Parser (CST)
import Language.Location (Span, Pos, pattern ZwSpan)

import qualified Data.List.NonEmpty as NE

------------------------
------ Core Types ------
------------------------

newtype Recog err a = R { unR :: St -> Result err (St, a) }

------ Introduction and Elimination Forms ------

runRecog :: (CST -> Recog err a) -> CST -> Result err a
runRecog action input = snd <$> unR (action input) st0
  where
  st0 = St
    { context = Nothing
    , rest = Done $ ZwSpan input.span.start
    }

-- | Examine a given value according to a recognition rule (eg pattern match function, predicate, &c).
-- The result is in the 'Recog' monad for easy composition.
--
-- This is a primitive introduction form for 'Recog'.
test :: (a -> Either err ok) -- ^ recognition rule
     -> a -- ^ value to examine
     -> Recog err ok
test f x = R $ \st -> case f x of
  Right y -> Result [] (Right (st, y))
  Left e -> Result [] (Left $ Leaf e)

-- | Consumes the next element in the (local, see 'parse') input stream.
-- Effectively, this is @uncons@ for recognizers.
-- In regex, it would be something like @.|$@.
-- It is expected that you would then either
--   accept the 'Span' as end-of-input (regex @$@), or
--   pass the @CST@ or a transformed version to 'test' to narrow the acceptable next input.
--
-- This is a primitive introduction form for 'Recog'.
pop :: Recog e (Either Span CST)
pop = R $ \st -> case st.rest of
  More (x:|y:ys) -> pure (st{rest = More $ y:|ys}, Right x)
  More (x:|[]) -> pure (st{rest = Done $ ZwSpan x.span.end}, Right x)
  Done loc -> pure (st, Left loc)

-- | Sets the input stream for the duration of a sub-recognizer.
-- While the sub-recognizer is evaluated it only has access to the input passed to 'parse'
-- When the sub-recognizer has finished, the input is restored to before is was invoked.
--
-- Note that 'runParser' invokes its recognizer with an empty input stream.
--
-- This is a primitive introduction form for 'Recog'.
parse :: (Foldable t)
  => (Pos, t CST) -- ^ zero or more trees
  -> Recog err a -- ^ action to parse the local input trees locally (without knowledge of the caller's trees)
  -> Recog err (a, [CST]) -- ^ result, plus any leftover trees
parse (p, trees) action = R $ \st -> do
  let inner = maybe (Done $ ZwSpan p) More $ NE.nonEmpty (toList trees)
  case unR action st{rest = inner} of
    Result errs1 (Right (st', x)) -> do
      let leftover = case st'.rest of
            Done _ -> []
            More (y:|ys) -> y:ys
      Result errs1 (Right (st, (x, leftover)))
    Result errs (Left err) -> Result errs (Left err)

-- see below for more error and state management "introduction" forms

------ Key Typeclasses ------

instance Functor (Recog e) where
  fmap f action = R $ \st -> do
    (st', x) <- unR action st
    pure (st', f x)

instance Applicative (Recog e) where
  pure x = R $ \st -> pure (st, x)
  getF <*> getX = R $ \st -> do
    (st', f) <- unR getF st
    (st'', x) <- unR getX st'
    pure (st'', f x)

instance Monad (Recog e) where
  getX >>= k = R $ \st -> do
    (st', x) <- unR getX st
    unR (k x) st'

-- | Implements alternation without requiring the 'empty' from 'Alternative'.
-- Uses backtracking search.
instance Semigroup (Recog e a) where
  a <> b = R $ \st -> case (unR a st, unR b st) of
    (Result errs1 (Right r), _) -> Result errs1 (Right r)
    (_, Result errs2 (Right r)) -> Result errs2 (Right r)
    (Result errs1 (Left e1), Result errs2 (Left e2)) ->
      -- FIXME take the error with the furthest position (I guess the Left of Result needs a position)
      Result [] (Left $ OrErrors (AndErrors $ snocNE errs1 e1)
                                 (AndErrors $ snocNE errs2 e2))

snocNE :: [a] -> a -> NonEmpty a
snocNE [] y = y:|[]
snocNE (x:xs) y = x:|(xs <> [y])

------ Internal State Tracking ------

data St = St
  { context :: Maybe Text
  , rest :: Input
  }

data Input
  = Done Span
  | More (NonEmpty CST)
instance HasField "curPos" Input Span where
  getField (Done l) = l
  getField (More (t:|_)) = t.span

data Result e a = Result
  { nonFatalErrors :: [ErrorTree e]
  , final :: Either (ErrorTree e) a
  }
  deriving (Functor)

instance Applicative (Result e) where
  pure x = Result [] (Right x)
  (<*>) = ap

instance Monad (Result e) where
  Result errs1 (Right x) >>= k = case k x of
    Result errs2 r -> Result (errs1 <> errs2) r
  Result errs1 (Left e) >>= _ = Result errs1 (Left e)

----------------------------
------ Error Handling ------
----------------------------

data ErrorTree e
  = Leaf e
  | BecauseError e (ErrorTree e)
  | And_ (ErrorTree e) (ErrorTree e)
  | OrErrors (ErrorTree e) (ErrorTree e)
  -- | The first is the initial error and the remaining are errors found after recovery
  | ThenError (ErrorTree e) (NonEmpty (ErrorTree e))
  deriving (Show)

pattern AndErrors :: NonEmpty (ErrorTree e) -> ErrorTree e
pattern AndErrors xs <- (NE.nonEmpty . errToList -> Just xs)
  where
  AndErrors (x:|[]) = x
  AndErrors (x:|y:ys) = And_ x (AndErrors (y:|ys))
errToList :: ErrorTree e -> [ErrorTree e]
errToList (And_ e1 e2) = errToList e1 <> errToList e2
errToList e = [e]

-- | Signal a fatal error while attempting to recognize.
-- For non-fatal errors, see 'report'.
--
-- This is a primitive introduction form for 'Recog' that manages the "bad path".
fail :: e -> Recog e any
fail e = test (const $ Left e) ()

-- | Signal a non-fatal error (or warning, or simple note) while attempting to recognize.
-- For fatal errors, see 'fail'.
--
-- This is a primitive introduction form for 'Recog' that manages the "bad path".
report :: e -> Recog e ()
report e = R $ \st -> Result [Leaf e] $ Right (st, ())

-- TODO
-- recover :: Recog e a -> Recog e (Either (ErrorTree e) a)
-- recover action = R $ \st -> case unR action st of
--   Result [] (Right (st', x)) -> pure (st', Right x)
--   Result errs1 (Left err) -> Result errs1 (Right (st', Right x))

-- TODO
-- catchR :: (ErrorTree e -> Either (ErrorTree e) b) -> Recog e a b -> Recog e a b
-- catchR handler p = R $ \x -> case unR p x of
--   Result errs (Right y) -> Result errs (Right y)
--   Result errs (Left e) -> Result errs (handler e)

-- TODO
-- labelR :: e -> Recog e a b -> Recog e a b
-- labelR msg = catchR (\_ -> Left $ Leaf msg)

-- TODO
-- explainR :: e -> Recog e a b -> Recog e a b
-- explainR msg = catchR (\e -> Left $ BecauseError msg e)

---------------------------
------ Position Info ------
---------------------------

-- TODO
-- getLocation

---------------------------------
------ Generic Combinators ------
---------------------------------

headM :: (Applicative m) => [a] -> (a -> m ()) -> m ()
headM [] = const $ pure ()
headM (x:_) = ($ x)

leftM :: (Applicative m) => Either z a -> (z -> m a) -> m a
leftM (Right x) = const $ pure x
leftM (Left x) = ($ x)

onLeftM :: (Applicative m) => (z -> m a) -> Either z a -> m a
onLeftM = flip leftM

optional :: (Foldable f) => Recog e (f a) -> Recog e (Maybe a)
optional action = do
  results <- toList <$> action
  pure $ case results of
    x:_ -> Just x
    [] -> Nothing

