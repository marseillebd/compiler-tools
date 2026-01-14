{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Language.CCS.Recognize.New
  ( Recog
  , runRecog
  , parse, pop
  , fail, report
  , test
  -- , raiseR, reportR
  -- , catchR, labelR, explainR

  , Result(..)
  , ErrorTree(..)
  ) where

import Prelude hiding (fail)

import Control.Arrow (Arrow(..), ArrowChoice(..), returnA, ArrowApply(..), ArrowMonad(..))
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable(..))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Profunctor (Profunctor(..))
import Data.Text (Text)
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Parser (CST(..), Atom(..), Encloser(..), Separator(..))
import Language.Location (Span, Pos, spanFromPos)
import Language.Text (SrcText)

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
    , rest = Left $ spanFromPos input.span.start
    }

test :: (a -> Either err ok) -> a -> Recog err ok
test f x = R $ \st -> case f x of
  Right y -> Result [] (Right (st, y))
  Left e -> Result [] (Left $ Leaf e)

pop :: Recog e (Either Span CST)
pop = R $ \st -> case st.rest of
  Right (x:|y:ys) -> pure (st{rest = Right $ y:|ys}, Right x)
  Right (x:|[]) -> pure (st{rest = Left $ spanFromPos x.span.end}, Right x)
  Left loc -> pure (st, Left loc)

parse :: (Foldable t)
  => (Pos, t CST) -- ^ zero or more trees
  -> Recog err a -- ^ parse the input trees locally (without knowledge of the caller's trees)
  -> Recog err (a, [CST]) -- ^ result, plus any leftover trees
parse (p, trees) action = R $ \st -> do
  let inner = maybe (Left $ spanFromPos p) Right $ NE.nonEmpty (toList trees)
  case unR action st{rest = inner} of
    Result errs1 (Right (st', x)) -> do
      let leftover = case st'.rest of
            Left _ -> []
            Right (y:|ys) -> y:ys
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
  , rest :: Either Span (NonEmpty CST)
  }

data Result e a = Result
  { nonFatalErrors :: [ErrorTree e]
  , final :: Either (ErrorTree e) a
  }
  deriving (Functor)

instance Applicative (Result e) where
  pure x = Result [] (Right x)
  Result errs1 (Right f) <*> Result errs2 (Right x) = Result (errs1 <> errs2) (Right $ f x)
  Result errs1 (Right _) <*> Result errs2 (Left e) = Result (errs1 <> errs2) (Left e)
  Result errs1 (Left e) <*> _ = Result errs1 (Left e)

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

fail :: e -> Recog e any
fail e = test (const $ Left e) ()

report :: e -> Recog e ()
report e = R $ \st -> Result [Leaf e] $ Right (st, ())


-- recover :: Recog e a -> Recog e (Either (ErrorTree e) a)
-- recover action = R $ \st -> case unR action st of
--   Result [] (Right (st', x)) -> pure (st', Right x)
--   Result errs1 (Left err) -> Result errs1 (Right (st', Right x))

------ Sequence Recognizer ------

-- -- elim: recognizing a list becomes an ordinary recognizer
-- parseR :: (Foldable t)
--   => (tok -> Span) -- ^ how to get the position of a token
--   -> RecogList e tok b -- ^ sequential parser
--   -> Recog e (Pos, t tok) (b, [tok]) -- ^ given initial position and an iterable of tokens, recognize the prefix and return the remaining tokens
-- parseR getSpan p = R $ \input -> case unRL p (st0 input) of
--   Result errs (Right (st', y)) -> case st'.rest of
--     rest -> Result errs (Right (y, rest))
--   Result errs (Left e) -> Result errs (Left e)
--   where
--   st0 (pos0, xs) = St
--     { getSpan = getSpan
--     , pos = pos0
--     , rest = toList xs
--     }

-- monadRL :: Recog e a b -> a -> RecogList e any b
-- monadRL action x = RL $ \st -> do
--   y <- unR action x
--   pure (st, y)

-- lookRL :: Recog e (Either Pos tok) r -> RecogList e tok r -- intro: create a RL from an R by uncoonsing the state's list
-- lookRL p = RL $ \st -> do
--   let (next_m, st') = unconsSt st
--       next = maybe pos Right next_m
--       pos = Left st'.pos
--   unR p next <&> \r -> (st', r)

-- posRL :: RecogList e tok Pos
-- posRL = RL $ \st -> Result [] $ Right (st, st.pos)

-- restRL :: RecogList e tok [tok]
-- restRL = RL $ \st -> pure $ case st.rest of
--   _:_ ->
--     let pos = (st.getSpan $ last st.rest).end
--         st' = st{ pos = pos, rest = [] }
--      in (st', st.rest)
--   [] -> (st, [])

-- --- Supporting Types ---

-- unconsSt :: St tok -> (Maybe tok, St tok)
-- unconsSt st = case st.rest of
--   t : ts -> (Just t, st
--     { pos = (st.getSpan t).end
--     , rest = ts
--     })
--   [] -> (Nothing, st)

-- -----------------------------
-- ------ Error Reporting ------
-- -----------------------------

-- catchR :: (ErrorTree e -> Either (ErrorTree e) b) -> Recog e a b -> Recog e a b
-- catchR handler p = R $ \x -> case unR p x of
--   Result errs (Right y) -> Result errs (Right y)
--   Result errs (Left e) -> Result errs (handler e)

-- labelR :: e -> Recog e a b -> Recog e a b
-- labelR msg = catchR (\_ -> Left $ Leaf msg)

-- explainR :: e -> Recog e a b -> Recog e a b
-- explainR msg = catchR (\e -> Left $ BecauseError msg e)

-- -----------------------------
-- ------ Key Typeclasses ------
-- -----------------------------

-- ------ Scalar Recognizers are Arrows ------

-- instance Category (Recog e) where
--   id = R $ pure
--   (R p) . (R q) = R $ \a -> q a >>= p

-- instance Arrow (Recog e) where
--   arr f = R $ \x -> pure (f x)
--   (R p) *** (R q) = R $ \(a, b) -> case (p a, q b) of
--     (Result errs1 (Right x), Result errs2 (Right y)) ->
--       Result (errs1 <> errs2) (Right (x, y))
--     (Result errs1 (Left e1), Result errs2 (Left e2)) ->
--       Result [] (Left $ AndErrors (errs1, e1) (errs2, e2))
--     (Result errs1 (Left e1), _) ->
--       Result errs1 (Left e1)
--     (_, Result errs2 (Left e2)) ->
--       Result errs2 (Left e2)

-- instance ArrowChoice (Recog e) where
--   R p ||| R q = R $ \case
--     Left a -> p a
--     Right a -> q a
--   R p +++ R q = R $ \case
--     Left a -> Left <$> p a
--     Right a -> Right <$> q a

-- instance Functor (Recog e a) where
--   fmap f (R p) = R $ \x -> f <$> p x

-- instance Profunctor (Recog e) where
--   dimap f g (R p) = R $ \x -> g <$> p (f x)

-- ------ The Arrow Syntax is Bad ------

-- -- | Haskell's extension for arrow syntax is, to be honest, bad.
-- -- Details follow, but suffice it to say that I'm implementing 'ArrowApply',
-- --   not because you should need it, but because the style is more familiar
-- --   (to Haskell programmers, but also to the compiler).
-- --
-- -- Use 'monadR' and 'arrowR' to translate between arrow types and monad syntax.
-- --
-- -- At time of writing (Jan 2026, ghc 9.12.2),
-- --   ghc does not warn abut unused variables bound in a @proc@.
-- -- I remember there being some talk of "a more sophisticated desugaring"
-- --   would reduce the generated code, but there wasn't much motion on it at the time.
-- -- I imagine there hasn't been any work on it, and I'm not going to trust (or spend the effort to check) ghc to generate good code here.
-- -- There's very little documentation on practical use of arrows;
-- --   I'm figuring it out, but users of CCS shouldn't have to.
-- -- In particular, it's confusing to see an undefined variable error
-- --   when that variable is @proc@-bound[1] rather than "ordinary"
-- -- Also, there's not much description of the @proc@ grammar outside the proposal.
-- -- Even if it were well-tested and -documented, @proc@ blocks read backwards to my eye.
-- -- Data flows top-to-bottom (awesome) but right-to-left (backwards from English).
-- -- In particular, I find myself constantly writing a little, then going to the start of the line to write some more.
-- -- I imagine reading is a little easier when you're used to it, but... this friction isn't really justifiable.
-- --
-- -- [1]: I don't even know what these variables are named, but they appear
-- --   (a) like @x@ in @proc x -> do@
-- --   (b) or like @x@ in @x <- myArr -< input@
-- instance ArrowApply (Recog e) where
--   app :: Recog e (Recog e a b, a) b
--   app = R $ \(k, x) -> unR k x

-- -- | A specialization of the 'Kleisli' arrow @a -> m b@
-- --   for @m ==> ArrowMonad (Recog e)@.
-- type RecogMonad e a b = a -> ArrowMonad (Recog e) b

-- monadR :: Recog e a b -> RecogMonad e a b
-- monadR action x = ArrowMonad $ R $ \() -> unR action x

-- -- | fliped 'monadR', for convenience
-- feedR :: a -> Recog e a b -> ArrowMonad (Recog e) b
-- feedR = flip monadR

-- arrowR :: (RecogMonad e a b) -> Recog e a b
-- arrowR getStart = R $ \a -> do
--   let (ArrowMonad start) = getStart a
--   unR start ()

