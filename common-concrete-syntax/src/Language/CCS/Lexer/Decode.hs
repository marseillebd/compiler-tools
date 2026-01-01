module Language.CCS.Lexer.Decode
  ( decode
  , Line(..)
  , EolType(..)
  , linesFrom
  , lines
  ) where

import Prelude hiding (lines)

import Data.Function ((&))
import Language.CCS.Error (placeholder, internalError)
import Language.Location (Pos, startPos, incLine)
import Language.Text (SrcText)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding.Error as Codec
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Codec
import qualified Language.Text as Src

--------------------
------ Decode ------
--------------------

decode :: LBS.ByteString -> LT.Text
decode bytes = placeholder
  $ bytes
  & Codec.decodeUtf8With Codec.lenientDecode -- invalid bytes are replaced with U+FFFD (replacement character), so downstream from this placeholder can't have a byte offset

-------------------------
------ Split Lines ------
-------------------------

data Line a = Line
  { line :: a
  , eol :: EolType
  }

data EolType
  = LF
  | CRLF
  | CR
  | Eof
  deriving (Eq, Show)

lines :: LT.Text -> [Line SrcText]
lines = linesFrom startPos

linesFrom :: Pos -> LT.Text -> [Line SrcText]
linesFrom _ LT.Empty = [] -- NOTE this means a zero-byte file will have an empty list of lines; anything else will have at least one line (possibly blank ofc)
linesFrom l str = Line
  { line = Src.fromPos l (LT.toStrict lazyContent)
  , eol
  } : maybe [] (linesFrom nextLine) rest
  where
  (lazyContent, eol, rest) = case LT.break (\c -> c == '\n' || c == '\r') str of
    (pre,                   LT.Empty) -> (pre, Eof,  Nothing)
    (pre,            '\n' LT.:< post) -> (pre, LF,   Just post)
    (pre, '\r' LT.:< '\n' LT.:< post) -> (pre, CRLF, Just post)
    (pre, '\r' LT.:<            post) -> (pre, CR,   Just post)
    _ -> internalError "expected an end of line sequence after preaking on `\\n|\\r`"
  nextLine = incLine l
