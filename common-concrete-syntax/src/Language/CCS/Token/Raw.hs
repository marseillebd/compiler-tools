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


data StringPart
  = PlainStr Text
  | Escape Text
  deriving (Show)

