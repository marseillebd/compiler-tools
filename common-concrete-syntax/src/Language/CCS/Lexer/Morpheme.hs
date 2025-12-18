module Language.CCS.Lexer.Morpheme
  ( CCS(..)
  , Token(..)
  , StrToken(..)
  , annotation
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

)
|]
-- TODO the argument for Symbol should enforce the invariants on how Symbols may be spelled

deriving instance Show Token
deriving instance Show StrToken

annotation :: Token -> Span
annotation (Symbol a) = a.span
annotation (Number a _ _ _ _ _) = a
annotation (Str a _ _ _) = a
annotation (MlDelim a) = a.span
annotation (MlContent a) = a.span
annotation (MlClose a) = a
annotation (Punctuation a _) = a
annotation (Eol a _) = a
annotation (Whitespace a) = a.span
annotation (Comment a) = a.span
annotation (Illegal a) = a.span

data PunctuationType
  = Open BracketType
  | Close BracketType
  | Comma | Dots Int | Colons Int | Semicolon
  | Backslash
  deriving (Eq, Show)
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
