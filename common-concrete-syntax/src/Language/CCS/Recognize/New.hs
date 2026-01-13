{-# LANGUAGE PatternSynonyms #-}

module Language.CCS.Recognize.New
  ( Recog
  , runRecog
  , satR
  , raiseR, reportR
  , catchR, labelR, explainR

  , RecogList
  , parseR
  , lookRL
  , posRL
  , restRL
  , monadRL

  , Result(..)
  , ErrorReport(..)

  , RecogMonad
  , monadR, feedR
  , arrowR
  ) where

import Control.Arrow (Arrow(..), ArrowChoice(..), returnA, ArrowApply(..), ArrowMonad(..))
import Control.Category (Category (..))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable(..))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Profunctor (Profunctor(..))
import Data.Text (Text)
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Parser (CST(..), Atom(..), Encloser(..), Separator(..))
import Language.Location (Span, Pos)
import Language.Text (SrcText)

------------------------
------ Core Types ------
------------------------

------ Scalar Recognizer ------

newtype Recog e a b = R { unR :: a -> Result e b }

runRecog :: Recog e a b -> a -> Result e b -- elim
runRecog = unR

satR :: (a -> Either e b) -> Recog e a b -- intro: transform the input to the output, or an error
satR f = R $ \x -> case f x of
  Right y -> Result [] (Right y)
  Left e -> Result [] (Left $ Leaf e)

raiseR :: Recog e e any
raiseR = R $ \e -> Result [] (Left $ Leaf e)

reportR :: Recog e e ()
reportR = R $ \e -> Result [Leaf e] (Right ())

------ Sequence Recognizer ------

newtype RecogList e a b = RL { unRL :: St a -> Result e (St a, b) }

-- elim: recognizing a list becomes an ordinary recognizer
parseR :: (Foldable t)
  => (tok -> Span) -- ^ how to get the position of a token
  -> RecogList e tok b -- ^ sequential parser
  -> Recog e (Pos, t tok) (b, [tok]) -- ^ given initial position and an iterable of tokens, recognize the prefix and return the remaining tokens
parseR getSpan p = R $ \input -> case unRL p (st0 input) of
  Result errs (Right (st', y)) -> case st'.rest of
    rest -> Result errs (Right (y, rest))
  Result errs (Left e) -> Result errs (Left e)
  where
  st0 (pos0, xs) = St
    { getSpan = getSpan
    , pos = pos0
    , rest = toList xs
    }

monadRL :: Recog e a b -> a -> RecogList e any b
monadRL action x = RL $ \st -> do
  y <- unR action x
  pure (st, y)

lookRL :: Recog e (Either Pos tok) r -> RecogList e tok r -- intro: create a RL from an R by uncoonsing the state's list
lookRL p = RL $ \st -> do
  let (next_m, st') = unconsSt st
      next = maybe pos Right next_m
      pos = Left st'.pos
  unR p next <&> \r -> (st', r)

posRL :: RecogList e tok Pos
posRL = RL $ \st -> Result [] $ Right (st, st.pos)

restRL :: RecogList e tok [tok]
restRL = RL $ \st -> pure $ case st.rest of
  _:_ ->
    let pos = (st.getSpan $ last st.rest).end
        st' = st{ pos = pos, rest = [] }
     in (st', st.rest)
  [] -> (st, [])

--- Supporting Types ---

data St tok = St
  { getSpan :: tok -> Span
  , pos :: Pos
  , rest :: [tok]
  }

unconsSt :: St tok -> (Maybe tok, St tok)
unconsSt st = case st.rest of
  t : ts -> (Just t, st
    { pos = (st.getSpan t).end
    , rest = ts
    })
  [] -> (Nothing, st)

-----------------------------
------ Error Reporting ------
-----------------------------

data Result e a = Result
  { recoveredFrom :: [ErrorReport e]
  , final :: Either (ErrorReport e) a
  }
  deriving (Functor)

instance Applicative (Result a) where
  pure x = Result [] (Right x)
  Result errs1 (Right f) <*> Result errs2 (Right x) = Result (errs1 <> errs2) (Right $ f x)
  Result errs1 (Right _) <*> Result errs2 (Left e) = Result (errs1 <> errs2) (Left e)
  Result errs1 (Left e) <*> _ = Result errs1 (Left e)

instance Monad (Result a) where
  Result errs1 (Right x) >>= k = case k x of
    Result errs2 r -> Result (errs1 <> errs2) r
  Result errs1 (Left e) >>= _ = Result errs1 (Left e)

data ErrorReport e
  = Leaf e
  | BecauseError e (ErrorReport e)
  | AndErrors ([ErrorReport e], ErrorReport e) ([ErrorReport e], ErrorReport e)
  | OrErrors (ErrorReport e) (ErrorReport e)
  deriving (Show)

catchR :: (ErrorReport e -> Either (ErrorReport e) b) -> Recog e a b -> Recog e a b
catchR handler p = R $ \x -> case unR p x of
  Result errs (Right y) -> Result errs (Right y)
  Result errs (Left e) -> Result errs (handler e)

labelR :: e -> Recog e a b -> Recog e a b
labelR msg = catchR (\_ -> Left $ Leaf msg)

explainR :: e -> Recog e a b -> Recog e a b
explainR msg = catchR (\e -> Left $ BecauseError msg e)

-----------------------------
------ Key Typeclasses ------
-----------------------------

------ Scalar Recognizers are Arrows ------

instance Category (Recog e) where
  id = R $ pure
  (R p) . (R q) = R $ \a -> q a >>= p

instance Arrow (Recog e) where
  arr f = R $ \x -> pure (f x)
  (R p) *** (R q) = R $ \(a, b) -> case (p a, q b) of
    (Result errs1 (Right x), Result errs2 (Right y)) ->
      Result (errs1 <> errs2) (Right (x, y))
    (Result errs1 (Left e1), Result errs2 (Left e2)) ->
      Result [] (Left $ AndErrors (errs1, e1) (errs2, e2))
    (Result errs1 (Left e1), _) ->
      Result errs1 (Left e1)
    (_, Result errs2 (Left e2)) ->
      Result errs2 (Left e2)

instance ArrowChoice (Recog e) where
  R p ||| R q = R $ \case
    Left a -> p a
    Right a -> q a
  R p +++ R q = R $ \case
    Left a -> Left <$> p a
    Right a -> Right <$> q a

-- | Implements alternation without requiring the 'empty' from 'Alternative'.
-- Uses backtracking search.
instance Semigroup (Recog e a b) where
  p <> q = R $ \x -> case (unR p x, unR q x) of
    (Result errs1 (Right b), _) -> Result errs1 (Right b)
    (_, Result errs2 (Right b)) -> Result errs2 (Right b)
    (Result errs1 (Left e1), Result errs2 (Left e2)) ->
      Result [] (Left $ OrErrors (noRecov errs1 e1) (noRecov errs2 e2))
    where

-- take the first error (all errors after the first recovered one are suspect
noRecov :: [ErrorReport e] -> ErrorReport e -> ErrorReport e
noRecov [] e = e
noRecov (e:_) _ = e

instance Functor (Recog e a) where
  fmap f (R p) = R $ \x -> f <$> p x

instance Profunctor (Recog e) where
  dimap f g (R p) = R $ \x -> g <$> p (f x)

------ The Arrow Syntax is Bad ------

-- | Haskell's extension for arrow syntax is, to be honest, bad.
-- Details follow, but suffice it to say that I'm implementing 'ArrowApply',
--   not because you should need it, but because the style is more familiar
--   (to Haskell programmers, but also to the compiler).
--
-- Use 'monadR' and 'arrowR' to translate between arrow types and monad syntax.
--
-- At time of writing (Jan 2026, ghc 9.12.2),
--   ghc does not warn abut unused variables bound in a @proc@.
-- I remember there being some talk of "a more sophisticated desugaring"
--   would reduce the generated code, but there wasn't much motion on it at the time.
-- I imagine there hasn't been any work on it, and I'm not going to trust (or spend the effort to check) ghc to generate good code here.
-- There's very little documentation on practical use of arrows;
--   I'm figuring it out, but users of CCS shouldn't have to.
-- In particular, it's confusing to see an undefined variable error
--   when that variable is @proc@-bound[1] rather than "ordinary"
-- Also, there's not much description of the @proc@ grammar outside the proposal.
-- Even if it were well-tested and -documented, @proc@ blocks read backwards to my eye.
-- Data flows top-to-bottom (awesome) but right-to-left (backwards from English).
-- In particular, I find myself constantly writing a little, then going to the start of the line to write some more.
-- I imagine reading is a little easier when you're used to it, but... this friction isn't really justifiable.
--
-- [1]: I don't even know what these variables are named, but they appear
--   (a) like @x@ in @proc x -> do@
--   (b) or like @x@ in @x <- myArr -< input@
instance ArrowApply (Recog e) where
  app :: Recog e (Recog e a b, a) b
  app = R $ \(k, x) -> unR k x

-- | A specialization of the 'Kleisli' arrow @a -> m b@
--   for @m ==> ArrowMonad (Recog e)@.
type RecogMonad e a b = a -> ArrowMonad (Recog e) b

monadR :: Recog e a b -> RecogMonad e a b
monadR action x = ArrowMonad $ R $ \() -> unR action x

-- | fliped 'monadR', for convenience
feedR :: a -> Recog e a b -> ArrowMonad (Recog e) b
feedR = flip monadR

arrowR :: (RecogMonad e a b) -> Recog e a b
arrowR getStart = R $ \a -> do
  let (ArrowMonad start) = getStart a
  unR start ()

------ Sequence Recognizers are Monads ------

instance Functor (RecogList e tok) where
  fmap f (RL p) = RL $ \st -> p st <&> \(st', x) -> (st', f x)

instance Applicative (RecogList e tok) where
  pure x = RL $ \st -> pure (st, x)
  getF <*> getX = RL $ \st -> do
    (st', f) <- unRL getF st
    (st'', x) <- unRL getX st'
    pure (st'', f x)

instance Monad (RecogList e tok) where
  getX >>= k = RL $ \st -> do
    (st', x) <- unRL getX st
    unRL (k x) st'

-- | Implements alternation without requiring the 'empty' from 'Alternative'.
-- Uses backtracking search.
instance Semigroup (RecogList e tok r) where
  a <> b = RL $ \st -> case (unRL a st, unRL b st) of
    (Result errs1 (Right r), _) -> Result errs1 (Right r)
    (_, Result errs2 (Right r)) -> Result errs2 (Right r)
    (Result errs1 (Left e1), Result errs2 (Left e2)) ->
      Result [] (Left $ OrErrors (noRecov errs1 e1) (noRecov errs2 e2))
