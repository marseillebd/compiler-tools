module DepC.Syntax
  ( Mod(..)
  , Def(..)
  , Typ(..)
  ) where

import Data.Text (Text)

data Mod = Mod
  { name :: Text
  , defs :: [Def]
  }

data Def
  = GlobalVar Text Typ -- TODO initializer?

data Typ
  = I32

-- data Proc = Proc
--   { name :: Text
--   , cname :: Text
--   -- TODO type
--   , body :: Expr -- NOTE hare compound expressions could be used to implement return statements
--   }


