-- DELME the whole token module, once I'm happy about doing so

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.CCS.Token.Raw
  ( CCS(..)
  , Token(..)
  , annotation
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
    (IntLit loc Integer)
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
-- TODO FloLit for floating point literals

deriving instance Show a => Show (Token a)
deriving instance Functor Token

annotation :: Token a -> a
annotation (Symbol a _) = a
annotation (IntLit a _) = a
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

data QuoteToken
  = SqlQuote
  | DblQuote
  | Backtick
  | MlQuote Text
  -- TODO multi-line string quote
  deriving (Eq, Show)

data StringPart
  = PlainStr Text
  | Escape Text
  deriving (Show)

