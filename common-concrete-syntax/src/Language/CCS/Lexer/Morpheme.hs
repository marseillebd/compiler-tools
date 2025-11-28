module Language.CCS.Lexer.Morpheme
  ( CCS(..)
  , Token(..)
  , annotation
  , PunctuationType(..)
  , BracketType(..)
  , Sign(..)
  , Radix(..)
  , QuoteType(..)
  , EolType(..)
  ) where

import Data.Text (Text)
import Language.Nanopass (deflang)

[deflang|
((CCS loc)

  (Token
    (Symbol loc Text)
    (Sign loc Sign)
    (Radix loc Radix)
    (Digits loc Integer Int)
    (Power loc)
    (Punctuation loc PunctuationType)
    (Quote loc QuoteType)
    (StdStr loc Text)
    (StrEscape loc (* Char))
    (Eol loc EolType)
    (Whitespace loc Text)
    (Comment loc Text)
    (Illegal loc Text)
  )

)
|]
-- TODO the argument for Symbol should enforce the invariants on how Symbols may be spelled

deriving instance Show a => Show (Token a)
deriving instance Functor Token

annotation :: Token a -> a
annotation (Symbol a _) = a
annotation (Sign a _) = a
annotation (Radix a _) = a
annotation (Digits a _ _) = a
annotation (Power a) = a
annotation (Punctuation a _) = a
annotation (Quote a _) = a
annotation (StdStr a _) = a
annotation (StrEscape a _) = a
annotation (Eol a _) = a
annotation (Whitespace a _) = a
annotation (Comment a _) = a
annotation (Illegal a _) = a

data PunctuationType
  = Open BracketType
  | Close BracketType
  | Comma | Dot | Colon | Semicolon
  | Backslash
  deriving (Eq, Show)
data BracketType = Round | Square | Curly
  deriving (Eq, Show)

data Sign = Positive | Negative
  deriving (Eq, Show)

data Radix = Base2 | Base8 | Base10 | Base16
  deriving (Eq, Show)

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
