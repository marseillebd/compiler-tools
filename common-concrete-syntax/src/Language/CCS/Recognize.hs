{-# LANGUAGE Arrows #-}

module Language.CCS.Recognize
  ( parenList, squareList, curlyBlock
  , parens, brackets, braces
  , semicolons, commas, spaced, chained, colons
  , unconsR, nilR
  ) where

import Prelude hiding (id, (.), fail, lines)

import Control.Arrow (Arrow (..), ArrowApply (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), (>>>), (<<<), returnA)
import Data.Semigroup (Semigroup(sconcat))
import Control.Category (Category (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Parser (CST(..), Atom(..), Encloser(..), Separator(..))
import Language.Location (Span)
import Language.Text (SrcText)
import Language.CCS.Recognize.Core (type (~>), enclosed, indented, separated, fail)
import Language.CCS.Recognize.Core (maybeR, manyR, someR)

import qualified Data.List.NonEmpty as NE

-- | A list of csts, separated by commas, enclosed in parenthesis.
-- Also allows for parenthesized indentation, where each line is a possibly comma-separated list of CSTs.
--
-- For example, this parser will accept and produce identical outputs for all of the following trees:
-- @
-- (1, 2, 3)
--
-- (
--   1
--   2
--   3
-- )
--
-- (
--   1, 2
--   3
--  )
-- @
parenList :: CST ~> [CST]
parenList = proc top -> do
  inner <- parens -< top
  elems <- maybeR [] (NE.toList <$> commas <+> indentedCommas) -< inner
  returnA -< elems

squareList :: CST ~> [CST]
squareList = proc top -> do
  inner <- brackets -< top
  elems <- maybeR [] (NE.toList <$> commas <+> indentedCommas) -< inner
  returnA -< elems

-- FIXME allow bare indent as well
curlyBlock :: CST ~> [CST]
curlyBlock = proc top -> do
  inner <- braces -< top
  elems <- maybeR [] (NE.toList <$> semicolons <+> indentedSemicolons) -< inner
  returnA -< elems

indentedCommas :: CST ~> NonEmpty CST
indentedCommas = proc top -> do
  lines <- indented -< top
  elems <- arr sconcat <<< someR (commas <+> arr NE.singleton) -< lines
  returnA -< elems

indentedSemicolons :: CST ~> NonEmpty CST
indentedSemicolons = proc top -> do
  lines <- indented -< top
  elems <- arr sconcat <<< someR (semicolons <+> arr NE.singleton) -< lines
  returnA -< elems

parens, brackets, braces :: CST ~> Maybe CST
parens = enclosed Round
brackets = enclosed Square
braces = enclosed Curly

semicolons, commas, spaced, chained, colons :: CST ~> NonEmpty CST
semicolons = separated Semicolon
commas = separated Comma
spaced = separated Space
chained = separated Chain
colons = separated Qualify

unconsR :: Text -> [a] ~> (a, [a])
unconsR msg = proc xs0 -> do
  case xs0 of
    x : xs -> returnA -< (x, xs)
    [] -> fail msg -< ()

nilR :: Text -> [a] ~> ()
nilR msg = proc xs0 -> do
  case xs0 of
    [] -> returnA -< ()
    _ -> fail msg -< ()
