module Language.CCS
  ( -- the main CCS data type
    CST(..)
  , Encloser(..)
  , Separator(..)
  , Atom(..)
  , FloLit(..)
  , Sign(..)
  , Radix(..)
  , CCS(..)
  -- supporting data types
  , Span, Pos, SrcText
  , startPos
  , EolType(..)
  , NonEmpty((:|))
  , Text
  -- parsing operations
  , cstsFrom, csts
  , tokensFrom, tokens
  -- TODO error classes
  , ReaderStyle(..)
  , ReaderError(..)
  , ReaderHooks(..)
  -- recognizers
  , Recognize, type (~>) , run
  , Context , Error(..) , Errors

  , Category(id, (.))
  , Arrow(..), ArrowApply(..), ArrowChoice(..), ArrowPlus(..), ArrowZero(..), returnA
  , (<<<), (>>>)
  , Profunctor(..)
  , maybeR
  , manyR
  , someR
  , unconsR, nilR

  , parenList, squareList
  , curlyBlock
  , fail
  , theSpan
  , atom
  , template
  , enclosed, parens, brackets, braces
  , indented
  , separated, semicolons, commas, pair, spaced, chained, colons
  , symbol, intLit, floLit, strLit, multilineLit
  ) where

import Prelude hiding (fail, id, (.))

import Language.CCS.Recognize.Core

import Control.Arrow (Arrow (..), ArrowApply (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), returnA, (<<<), (>>>))
import Control.Category (Category (..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Language.CCS.Error (ReaderStyle(..), ReaderError(..), ReaderHooks(..))
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Lexer.Cover (Sign(..), Radix(..))
import Language.CCS.Lexer (cstsFrom, csts, tokensFrom, tokens)
import Language.CCS.Lexer.Decode (EolType(..))
import Language.CCS.Parser (CCS(..), CST(..), Atom(..), Encloser(..), Separator(..))
import Language.CCS.Recognize (parenList, squareList, curlyBlock, parens, brackets, braces, semicolons, commas, spaced, chained, colons, unconsR, nilR)
import Language.Location (Span, Pos, startPos)
import Language.Text (SrcText)
