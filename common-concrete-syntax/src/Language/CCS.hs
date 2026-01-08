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
  , LexerError
  , ParseError(..)
  , RaiseIllegalBytes(..)
  , DeleteComment(..)
  , MalformedPunctuation(..)
  , MalformedNumber(..)
  , MalformedString(..)
  , MalformedIndentation(..)
  , WhitespaceError(..), InconsistentNewlines(..)
  , SandhiError(..)
  -- recognizers
  , Recognize, type (~>) , run
  , Context , Error(..) , Errors

  , Category(id, (.))
  , Arrow(arr, (***)), ArrowApply(..), ArrowChoice(..), ArrowPlus(..), ArrowZero(..), returnA
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

import Language.CCS.Recognize (parenList, squareList, curlyBlock, parens, brackets, braces, semicolons, commas, spaced, chained, colons, unconsR, nilR)
import Control.Arrow (Arrow (arr, (***)), ArrowApply (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), returnA)
import Control.Category (Category (..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Lexer.Assemble (MalformedPunctuation(..), MalformedNumber(..), MalformedString(..))
import Language.CCS.Lexer.Cover (Sign(..), Radix(..))
import Language.CCS.Lexer (cstsFrom, csts, tokensFrom, tokens, LexerError)
import Language.CCS.Lexer.Decode (EolType(..))
import Language.CCS.Lexer.Indentation (MalformedIndentation(..))
import Language.CCS.Lexer.NoiseReduction (DeleteComment(..), RaiseIllegalBytes(..), WhitespaceError(..), InconsistentNewlines(..))
import Language.CCS.Lexer.Sandhi (SandhiError(..))
import Language.CCS.Parser (CCS(..), CST(..), Atom(..), Encloser(..), Separator(..), ParseError(..))
import Language.Location (Span, Pos, startPos)
import Language.Text (SrcText)
