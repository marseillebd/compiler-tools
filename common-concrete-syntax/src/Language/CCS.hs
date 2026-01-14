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
  ) where

import Prelude hiding (fail, id, (.))

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Language.CCS.Error (ReaderStyle(..), ReaderError(..), ReaderHooks(..))
import Language.CCS.Lexer.Assemble (FloLit(..))
import Language.CCS.Lexer.Cover (Sign(..), Radix(..))
import Language.CCS.Lexer (cstsFrom, csts, tokensFrom, tokens)
import Language.CCS.Lexer.Decode (EolType(..))
import Language.CCS.Parser (CCS(..), CST(..), Atom(..), Encloser(..), Separator(..))
import Language.Location (Span, Pos, startPos)
import Language.Text (SrcText)
