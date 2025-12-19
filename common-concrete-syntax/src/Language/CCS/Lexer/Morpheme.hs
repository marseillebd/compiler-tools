module Language.CCS.Lexer.Morpheme
  ( CCS(..)
  , Token(..)
  , StrToken(..)
  , PunctuationType(..)
  , BracketType(..)
  , Sign(..)
  , Radix(..)
  , QuoteType(..)
  , EolType(..)
  ) where

import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.Location (Span)
import Language.Nanopass (deflang)
import Language.Text (SrcText)

[deflang|
(CCS

  (Token
    (Symbol SrcText)
    (Number Span
      Sign
      Radix
      Integer
      (? (& Integer Int))
      (? Integer)
    )
    (Str Span QuoteType (* StrToken) (? QuoteType))
    (MlDelim SrcText)
    (MlContent SrcText)
    (MlClose Span)
    (Punctuation Span PunctuationType)
    (Eol Span EolType)
    (Whitespace SrcText)
    (Comment SrcText)
    (Illegal SrcText)
  )

  (StrToken
    (StdStr SrcText)
    (StrEscape Span Char)
    (IllStr SrcText)
  )

  (PunctuationType
    (Open BracketType)
    (Close BracketType)
    (Dots Int)
    (Colons Int)
    (Comma)
    (Semicolon)
    (Backslash)
  )

)
|]
-- TODO the argument for Symbol should enforce the invariants on how Symbols may be spelled

deriving instance Show Token
deriving instance Show StrToken
deriving instance Show PunctuationType
deriving instance Eq PunctuationType

instance HasField "span" Token Span where
  getField (Symbol a) = a.span
  getField (Number a _ _ _ _ _) = a
  getField (Str a _ _ _) = a
  getField (MlDelim a) = a.span
  getField (MlContent a) = a.span
  getField (MlClose a) = a
  getField (Punctuation a _) = a
  getField (Eol a _) = a
  getField (Whitespace a) = a.span
  getField (Comment a) = a.span
  getField (Illegal a) = a.span

data BracketType = Round | Square | Curly
  deriving (Eq, Show)

data Sign = Positive | Negative
  deriving (Eq, Show)

data Radix = Base2 | Base8 | Base10 | Base16
  deriving (Eq, Show)

instance HasField "base" Radix Integer where
  getField = \case
    Base2 -> 2
    Base8 -> 8
    Base10 -> 10
    Base16 -> 16

data QuoteType
  = SqlQuote
  | DblQuote
  | Backtick
  | MlQuote Text
  deriving (Eq, Show)

data EolType
  = LF
  | CRLF
  | CR
  | Eof
  deriving (Eq, Show)
