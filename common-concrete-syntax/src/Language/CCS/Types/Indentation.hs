module Language.CCS.Types.Indentation where

import GHC.Records (HasField(..))
import Language.Location (Span)
import Language.Nanopass (deflang)
import Language.Text (SrcText)

import qualified Language.CCS.Types.Assemble as L0

[deflang|
(CCS from L0:CCS
  (* Atom
    (- MultilineLiteral)
    (+ MultilineLiteral (* SrcText))
  )

  (* Token
    (- Eol)
    (+ Indent Span)
    (+ Nextline Span)
    (+ Dedent Span)
  )
)
|]

deriving instance Show Atom
deriving instance Show Token
deriving instance Show PunctuationType

instance HasField "span" Token Span where
  getField (Atom a _) = a
  getField (StringTemplate a _ _) = a
  getField (Punctuation a _) = a
  getField (Whitespace a) = a.span
  getField (Indent a) = a
  getField (Nextline a) = a
  getField (Dedent a) = a
