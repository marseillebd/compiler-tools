module Language.CCS.Error
  ( placeholder
  , unimplemented
  , internalError
  , Unwrap(..)
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
  url = "https://github.com/marseillebd/compiler-tools" -- FIXME be sure to update this url

class Unwrap f where
  unwrapOr :: a -> f a -> a
  -- TODO should the *OrPanic* functions have warnings associated with them?
  unwrapOrPanic :: String -> f a -> a
  unwrapOrPanic msg = unwrapOr (internalError msg)
  unwrapOrPanic_ :: f a -> a
  unwrapOrPanic_ = unwrapOrPanic "<no message>"

instance Unwrap Maybe where
  unwrapOr x = maybe x id
