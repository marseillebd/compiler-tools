module Language.CCS.Error
  ( placeholder
  , unimplemented
  , internalError
  , Unwrap(..)
  , unused
  ) where

placeholder :: a -> a
{-# INLINE placeholder #-}
{-# WARNING placeholder "this is a placeholder implementation" #-}
placeholder x = x

unimplemented :: String -> a
{-# WARNING unimplemented "part of the implementation is missing" #-}
unimplemented msg = error $ "[UNIMPLEMENTED]: " ++ msg

internalError :: String -> a
internalError msg = error $ unlines
  [ "[INTERNAL ERROR]: " ++ msg
  , "please report at " ++ url
  ]
  where
  url :: String
  url = "https://github.com/marseillebd/compiler-tools" -- NOTE be sure to update this url

-- | Use like `_ignore = unused (foo, bar)` at top-level to opt-out of warnings about `foo` and `bar` being unused.
--
unused :: a -> ()
unused _ = ()

class Unwrap f where
  unwrapOr :: a -> f a -> a
  -- TODO should the *OrPanic* functions have warnings associated with them?
  unwrapOrPanic :: String -> f a -> a
  unwrapOrPanic msg = unwrapOr (internalError msg)
  unwrapOrPanic_ :: f a -> a
  unwrapOrPanic_ = unwrapOrPanic "<no message>"

instance Unwrap Maybe where
  unwrapOr x = maybe x id

