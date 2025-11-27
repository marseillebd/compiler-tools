-- DELME the whole token module, once I'm happy about doing so

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.CCS.Token.Raw
  ( CCS(..)
  , Token(..)
  , annotation
  , Sign(..)
  , Radix(..)
  , PunctToken(..)
  , Enclose(..)
  , QuoteToken(..)
  , StringPart(..)
  ) where


import Data.Text (Text)
import Language.Nanopass (deflang)

[deflang|
((CCS loc)

  (Token
    (Symbol loc Text)
    (SignTok loc Sign)
    (Radix loc Radix)
    (Digits loc Integer Int)
    (Power loc)
    (Punctuation loc PunctToken)
    (Quote loc QuoteToken)
    (StdStr loc Text)
    (StrEscape loc (* Char))
    (Newline loc)
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
annotation (SignTok a _) = a
annotation (Radix a _) = a
annotation (Digits a _ _) = a
annotation (Power a) = a
annotation (Punctuation a _) = a
annotation (Quote a _) = a
annotation (StdStr a _) = a
annotation (StrEscape a _) = a
annotation (Newline a) = a
annotation (Whitespace a _) = a
annotation (Comment a _) = a
annotation (Illegal a _) = a


data PunctToken
  = Open Enclose
  | Close Enclose
  | Comma | Dot | Colon | Semicolon
  | Backslash
  deriving (Eq, Show)
data Enclose = Round | Square | Curly
  deriving (Eq, Show)

data Sign = Positive | Negative
  deriving (Eq, Show)

data Radix = Base2 | Base8 | Base10 | Base16
  deriving (Eq, Show)

data FloLit
  = Zero
  | NegZero
  | BinFloat
    { magnitude :: Integer
    , exponent :: Integer
    }
  | DecFloat
    { magnitude :: Integer
    , exponent :: Integer
    }
  deriving (Eq, Show)


data QuoteToken
  = SqlQuote
  | DblQuote
  | Backtick
  | MlQuote Text
  deriving (Eq, Show)

data StringPart
  = PlainStr Text
  | Escape Text
  deriving (Show)

