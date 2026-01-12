module Language.CCS.Types.Assemble where

import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.CCS.Lexer.Cover (Sign(..), Radix(..))
import Language.Location (Span)
import Language.Nanopass (deflang)
import Language.Text (SrcText)

import qualified Language.CCS.Types.NoiseReduction as L0

[deflang|
(CCS from L0:CCS
  (+ Atom
    (Symbol Text)
    (IntegerLiteral Integer)
    (FloatingLiteral FloLit)
    (StringLiteral Text)
    (MultilineLiteral (* SrcText) SrcText)
  )

  (* Token
    (- Symbol)
    (- Number)
    (- Str)

    (- MlDelim)
    (- MlContent)
    (- MlClose)

    (+ Atom Span Atom)
    (+ StringTemplate Span TemplateType Text)
  )

  (- StrToken)

  (* PunctuationType
    (- Dots)
    (+ Dot) (+ Dots2) (+ Dots3)
    (- Colons)
    (+ Colon) (+ Colons2) (+ Colons3)
  )
)
|]

data FloLit
  = FloLit
    { signF :: Sign
    , magF :: Integer -- NOTE should be Natural
    , expF :: (Radix, Integer)
    }
  deriving (Eq)

instance Show FloLit where
  show it = concat
    [ case it.signF of { Positive -> "+"; Negative -> "-" }
    , show it.magF
    , if snd it.expF == 0 then ""
      else concat
      [ "x"
      , show (fst it.expF).base
      , "^"
      , show $ snd it.expF
      ]
    ]

data TemplateType
  = OpenTemplate
  | MidTemplate
  | CloseTemplate
  deriving (Eq, Show)

deriving instance Show Atom
deriving instance Show Token
deriving instance Show PunctuationType

instance HasField "span" Token Span where
  getField (Atom a _) = a
  getField (Punctuation a _) = a
  getField (Whitespace a) = a.span
  getField (Eol a _) = a
  getField (StringTemplate a _ _) = a

