module Language.CCS.Types.Sandhi where

import GHC.Records (HasField(..))
import Language.Location (Span)
import Language.Nanopass (deflang)

import qualified Language.CCS.Types.Indentation as L0

[deflang|
(CCS from L0:CCS
  (* Token
    (- Whitespace)
  )
  (* PunctuationType
    (- Dot) (- Dots2) (- Dots3)
    (+ StartBlock)
    (+ Chain)
    (- Colon) (- Colons2) (- Colons3)
    (+ Pair) (+ Qualify)
    (- Backslash)
    (+ ContinueLine)
  )
)
|]

deriving instance Show Atom
deriving instance Show Token
deriving instance Show PunctuationType
deriving instance Eq PunctuationType

instance HasField "span" Token Span where
  getField (Atom a _) = a
  getField (StringTemplate a _ _) = a
  getField (Punctuation a _) = a
  getField (Indent a) = a
  getField (Nextline a) = a
  getField (Dedent a) = a

