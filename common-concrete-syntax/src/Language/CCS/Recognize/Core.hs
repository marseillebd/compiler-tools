{-# LANGUAGE Arrows #-}

module Language.CCS.Recognize.Core
  ( type (~>)
  , Recognize
  , run
  , Context
  , Error(..)
  , Errors

  , fail
  , theSpan
  , atom
  , template
  , enclosed
  , indented
  , separated, pair
  , symbol, intLit, floLit, strLit, multilineLit

  , maybeR
  , manyR
  , someR
  ) where

import Prelude hiding ((.), fail)

import Control.Arrow (Arrow (arr, (***)), ArrowApply (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), returnA)
import Control.Category (Category (..))
import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Parser (CST(..), Atom(..), Encloser(..), Separator(..))
import Language.Location (Span)
import Language.Text (SrcText)

import qualified Data.List.NonEmpty as NE

------------------------------------
------ Preliminary Data Types ------
------------------------------------

type (~>) = Recognize

newtype Recognize a b = R { unR ::
  Context ->
  -- ^ the location whence the value under inspection was extracted
  a ->
  -- ^ the value we are insepcting
  Either Errors (Context, b)
}

type Context = Span -- TODO perhaps this needs to be a bit more 

data Errors = ErrorsOne Error | ErrorsPlus Errors Errors
instance Semigroup Errors where
  (<>) = ErrorsPlus
data Error = Error
  { message :: Text
  , context :: Context
  }

run :: (CST ~> r) -> CST -> Either Errors r
run (R p) x = snd <$> p x.span x

-------------------------------
------ Primitive Parsers ------
-------------------------------

theSpan :: a ~> Span
theSpan = R $ \ctx _ -> Right (ctx, ctx)

_fail :: Context -> Text -> Either Errors a
_fail ctx msg = Left $ ErrorsOne $ Error msg ctx

------ Recognize CSTs ------

atom :: CST ~> Atom
atom = R $ \_ -> \case
  Atom loc a -> Right (loc, a)
  other -> _fail other.span "expecting atom"

template :: CST ~> (Text, NonEmpty (CST, Text))
template = R $ \_ -> \case
  Template loc txt rest -> Right (loc, (txt, rest))
  other -> _fail other.span "expecting string template"

-- TODO enclose (round, square, curly)
enclosed :: Encloser -> CST ~> Maybe CST
enclosed e = R $ \_ -> \case
  Enclose loc e' inner | e == e' -> Right (loc, inner)
  other -> _fail other.span $ "expecting " <> expect
  where
  expect = case e of
    Round -> "parentheses"
    Square -> "square brackets"
    Curly -> "curly braces"

indented :: CST ~> NonEmpty CST
indented = R $ \_ -> \case
  Block loc inner -> Right (loc, inner)
  other -> _fail other.span "expecting indented block"

separated :: Separator -> CST ~> NonEmpty CST
separated s = R $ \_ -> \case
  List loc s' xs | s == s' -> Right (loc, xs)
  other -> _fail other.span $ "expecting " <> msg
  where
  msg = case s of
    Semicolon -> "semicolon-separated list"
    Comma -> "comma-separated list"
    Space -> "space-separated trees"
    Chain -> "chained trees"
    Qualify -> "colon-separated atoms"

pair :: CST ~> (CST, CST)
pair = R $ \_ -> \case
  Pair loc k v -> Right (loc, (k, v))
  other -> _fail other.span "expecting colon-separated pair"

------ Recognize Atoms ------

symbol :: Atom ~> Text
symbol = R $ \ctx -> \case
  Symbol x -> Right (ctx, x)
  _ -> _fail ctx "expecting symbol"

intLit :: Atom ~> Integer
intLit = R $ \ctx -> \case
  IntegerLiteral i -> Right (ctx, i)
  _ -> _fail ctx "expecting integer literal"

floLit :: Atom ~> FloLit
floLit = R $ \ctx -> \case
  FloatingLiteral f -> Right (ctx, f)
  _ -> _fail ctx "expecting floating-point literal"

strLit :: Atom ~> Text
strLit = R $ \ctx -> \case
  StringLiteral txt -> Right (ctx, txt)
  _ -> _fail ctx "expecting string literal"

multilineLit :: Atom ~> [SrcText]
multilineLit = R $ \ctx -> \case
  MultilineLiteral txts -> Right (ctx, txts)
  _ -> _fail ctx "expecting multiline literal"

------ Structural ------

fail :: Text -> a ~> b
fail msg = R $ \ctx _ -> Left (ErrorsOne (Error msg ctx))

instance Functor (Recognize a) where
  fmap f (R p) = R $ \ctx x -> case p ctx x of
    Right (ctx', y) -> Right (ctx', f y)
    Left err -> Left err

instance Profunctor Recognize where
  dimap g f (R p) = R $ \ctx x -> case p ctx (g x) of
    Right (ctx', y) -> Right (ctx', f y)
    Left err -> Left err

instance Applicative (Recognize a) where
  pure x = R $ \ctx _ -> Right (ctx, x)
  (R p) <*> (R q) = R $ \ctx x -> case (p ctx x, q ctx x) of
    (Right (_, f), Right (_, y)) -> Right (ctx, f y)
    (Left err, _) -> Left err
    (_, Left err) -> Left err

instance Category Recognize where
  id = R $ \ctx x -> Right (ctx, x)
  (R q) . (R p) = R $ \ctx x -> case p ctx x of
    Right (ctx', y) -> q ctx' y
    Left err -> Left err

instance Arrow Recognize where
  arr f = R $ \ctx x -> Right (ctx, f x)
  (R p) *** (R q) = R $ \ctx (x, y) -> case (p ctx x, q ctx y) of
    (Right (_, x'), Right (_, y')) -> Right (ctx, (x', y'))
    (Left err, _) -> Left err
    (_, Left err) -> Left err

instance ArrowZero Recognize where
  zeroArrow = unknownFail

instance ArrowPlus Recognize where
  (R p) <+> (R q) = R $ \ctx x -> case p ctx x of
    Right success -> Right success
    Left errLeft -> case q ctx x of
      Right success -> Right success
      Left errRight -> Left $! (errLeft <> errRight)

instance ArrowChoice Recognize where
  (R p) +++ (R q) = R $ \ctx -> \case
    Left x -> case p ctx x of
      Right (ctx', y) -> Right (ctx', Left y)
      Left err -> Left err
    Right x -> case q ctx x of
      Right (ctx', y) -> Right (ctx', Right y)
      Left err -> Left err

instance ArrowApply Recognize where
  app = R $ \ctx (p, x) -> unR p ctx x

unknownFail :: a ~> b
unknownFail = R $ \ctx _ -> Left (ErrorsOne (Error "unknown error" ctx))

-------------------------
------ Combinators ------
-------------------------

-- TODO I had hooped that most of these would be implemented as simple arrows.,
-- but I can't easily find them, so I'll write them in the core for my own speed.
-- Perhaps someday I'll know this stuff enough to improve the offerings in `base`.

maybeR :: b -> (CST ~> b) -> Maybe CST ~> b
maybeR z f = R $ \cxt -> \case
  Nothing -> Right (cxt, z)
  Just x -> unR f x.span x

-- | More like a `map`, but it's not a fullly-general over functors or traversables, and I want one that works over nonempty, probablly
manyR :: (CST ~> b) -> [CST] ~> [b]
manyR (R p) = R $ \cxt xs ->
  let (errs, ys) = partitionEithers $ xs <&> \x ->
        second snd $ p x.span x
   in case errs of
    [] -> Right (cxt, ys)
    e : es -> Left $ foldr (flip (<>)) e es

-- | More like a `map`, but it's not a fullly-general over functors or traversables, and I want one that works over nonempty, probablly
someR :: (CST ~> b) -> NonEmpty CST ~> NonEmpty b
someR f = proc xs -> do
  ys0 <- manyR f -< NE.toList xs
  case ys0 of
    [] -> unknownFail -< ys0
    y : ys -> returnA -< y :| ys


