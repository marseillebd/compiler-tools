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
  , parens, brackets, braces
  , indented
  , semicolons, commas, pair, spaced, chained, colons
  , symbol , intLit , floLit , strLit, multilineLit
  ) where

import Prelude hiding ((.), fail)

import Control.Arrow (Arrow (..), ArrowApply (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), (>>>))
import Control.Category (Category (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Parser (CST(..), Atom(..), Encloser(..), Separator(..))
import Language.Location (Span)
import Language.Text (SrcText)

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
_enclosed :: Encloser -> CST ~> Maybe CST
_enclosed e = R $ \_ -> \case
  Enclose loc e' inner | e == e' -> Right (loc, inner)
  other -> _fail other.span $ "expecting " <> expect
  where
  expect = case e of
    Round -> "parentheses"
    Square -> "square brackets"
    Curly -> "curly braces"
parens, brackets, braces :: CST ~> Maybe CST
parens = _enclosed Round
brackets = _enclosed Square
braces = _enclosed Curly

indented :: CST ~> NonEmpty CST
indented = R $ \_ -> \case
  Block loc inner -> Right (loc, inner)
  other -> _fail other.span "expecting indented block"

semicolons :: CST ~> NonEmpty CST
semicolons = R $ \_ -> \case
  List loc Semicolon xs -> Right (loc, xs)
  other -> _fail other.span "expecting semicolon-separated list"

commas :: CST ~> NonEmpty CST
commas = R $ \_ -> \case
  List loc Comma xs -> Right (loc, xs)
  other -> _fail other.span "expecting comma-separated list"

pair :: CST ~> (CST, CST)
pair = R $ \_ -> \case
  Pair loc k v -> Right (loc, (k, v))
  other -> _fail other.span "expecting colon-separated pair"

spaced :: CST ~> NonEmpty CST
spaced = R $ \_ -> \case
  List loc Space xs -> Right (loc, xs)
  other -> _fail other.span "expecting space-separated trees"

chained :: CST ~> NonEmpty CST
chained = R $ \_ -> \case
  List loc Chain xs -> Right (loc, xs)
  other -> _fail other.span "expecting chained trees"

colons :: CST ~> NonEmpty CST
colons = R $ \_ -> \case
  List loc Qualify xs -> Right (loc, xs)
  other -> _fail other.span "expecting colon-separated atoms"

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
