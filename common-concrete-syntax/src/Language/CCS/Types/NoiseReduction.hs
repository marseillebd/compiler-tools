module Language.CCS.Types.NoiseReduction where

import GHC.Records (HasField(..))
import Language.Location (Span)
import Language.Nanopass (deflang)

import qualified Language.CCS.Lexer.Cover as L0

[deflang|
(CCS from L0:CCS
  (* Token
    (- Comment)
    (- Illegal)
  )
  (* StrToken
    (- IllStr)
  )
)
|]

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
  getField (Whitespace a) = a.span
  getField (Eol a _) = a
