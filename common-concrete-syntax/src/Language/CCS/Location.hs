module Language.CCS.Location
  ( Off(..)
  , Span(..)
  ) where

data Off = Off
  -- , byte :: !Int -- 0-indexed -- TODO after I implement my own decode stream
  { line :: !Int -- 1-indexed
  , col :: !Int -- 1-indexed
  }
  deriving (Show)

data Span = Span
  { start :: Off
  , end :: Off
  }
  deriving (Show)

-- TODO a type that adds a filepath (or stdin, or whatever) element to a location-y thing

-- TODO a type for describing locations even when macros are involved
-- data Loc = Loc (NonEmpty Span)
--   deriving (Show)

