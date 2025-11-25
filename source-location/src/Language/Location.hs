-- TODO incorporate the range overlap analysis from eexprs
-- see: https://github.com/marseillebd/eexprs/blob/c75ca4f829728d9ad978f531958c2acaf01c6086/hs/eexpr/src/Numeric/Interval/Compare.hs
module Language.Location
  ( -- * Offsets
    -- $offsets
    Off
  , zeroIndexed
  , oneIndexed
  , zeroOff
  , incOff
  , fromZero
  -- * Positions
  -- $positions
  , Pos
  , startPos
  , incCol
  , incLine
  -- * Spans
  -- $spans
  , Span
  , spanFromPos
  , mkSpan
  ) where

import Data.Function (on)
import GHC.Records (HasField(..))
import Text.ParserCombinators.ReadPrec (pfail)
import Text.Read (Read(readPrec))

import qualified Text.ParserCombinators.ReadPrec as Read

---------------------
------ Offsets ------
---------------------

-- $offsets
--
-- Normally, lines and columns are encoded as integers.
-- However, integers are a code smell, so we have 'Offset'
--
-- It offers access via both zero- and one-indexing.
-- (Internally, ther zero-indexed version is stored.)
-- To help avoid precondition issues, only 
-- The 'Show'/'Read' instances are one-indexed, because I exepct humans to read them.
-- I suggest that doing arithmetic on offsets intended to locate text is not reasonable.
-- We instead suggest using 'zeroOff' and 'incOff', with 'fromZero' for special initialization circumstances.
-- Yes, you could use these primitives to implement arithmetic, but that sounds tedious.

newtype Off = Off { zeroIndexed :: Word }
  deriving (Eq, Ord)
oneIndexed :: Off -> Word
oneIndexed l = l.zeroIndexed + 1
instance HasField "oneIndexed" Off Word where getField = oneIndexed

instance Show Off where showsPrec p l = showsPrec p l.oneIndexed
instance Read Off where
  readPrec = readPrec @Word >>= \case
    0 -> pfail
    i -> pure $ Off (i - 1)

zeroOff :: Off
zeroOff = fromZero 0

incOff :: Off -> Off
incOff = fromZero . (+1) . zeroIndexed

fromZero :: Word -> Off
fromZero = Off

-----------------------
------ Positions ------
-----------------------

-- $positions
--
-- Compilers need a way to track locations within a file.
-- Traditionally, this is done with 'Line' and 'Column' numbers.
--
-- There is some subtlety in what constitutes a location in source code.
-- Here, we define 'Position' as indicating a single point between two characters
-- (or just before the first or just after the last character).
--
-- Note that a 'Position' does NOT identify a character.
-- One can insert text at a position (moving subsequent text to larger positions),
-- or delete characters before or after a position.
-- This allows us to say things like:
-- \"we should have seen a <token> at <position>, but it is missing\".
-- To identify one or more characters in the source, see 'Span'.

newtype Line = Line { unLine :: Off }
  deriving (Eq, Ord)

instance Show Line where showsPrec p l = showsPrec p l.unLine
instance Read Line where readPrec = Line <$> readPrec @Off

newtype Column = Col { unCol :: Off }
  deriving (Eq, Ord)

instance Show Column where showsPrec p l = showsPrec p l.unCol
instance Read Column where readPrec = Col <$> readPrec @Off

data Pos = Pos
  -- , byte :: !Off -- TODO after I implement my own decode stream
  { line :: !Line
  , col :: !Column
  }
  deriving (Eq)

instance Show Pos where
  showsPrec p pos rest
    = showsPrec p pos.line
    $ (':':)
    $ showsPrec p pos.col
    $ rest
instance Read Pos where
  readPrec = do
    line <- readPrec @Line
    Read.get >>= \case { ':' -> pure (); _ -> pfail }
    col <- readPrec @Column
    pure Pos{line, col}

instance Ord Pos where
  compare a b = case (compare `on` line) a b of
    EQ -> (compare `on` col) a b
    it -> it

startPos :: Pos
startPos = Pos (Line zeroOff) (Col zeroOff)

incCol :: Pos -> Pos
incCol p = p{ col = Col $ incOff p.col.unCol }

incLine :: Pos -> Pos
incLine p = Pos
  { col = Col zeroOff
  , line = Line $ incOff p.line.unLine
  }

-------------------
------ Spans ------
-------------------

-- $spans
--
-- There is some subtlety in what constitutes a location in source code.
-- Here, we define a 'Span' as a range of characters between two 'Pos'isions.
-- Thus, you can think of a 'Span' as something like a cursor/selection in a text editor.
--
-- A 'Span' is comprised of a start 'Pos', and an end 'Pos'
-- It is subject to the invariant that the start is less than or equal to the end.
-- We encapsulate the representation however, to protect that invariant.
-- The functions 'spanFromPos' and 'mkSpan' are the safest intriduction forms.
--
-- 'Span' allows us to specify both empty and non-empty ranges of text.
-- Thus, we can use them to easily extract, replace, or delete contiguous sections of source code.

data Span = Span
  { _start :: Pos
  , _end :: Pos
  }
instance HasField "start" Span Pos where getField = _start
instance HasField "end" Span Pos where getField = _end

spanFromPos :: Pos -> Span
spanFromPos pos = Span pos pos

mkSpan :: Pos -> Pos -> Maybe Span
mkSpan a b
  | a <= b = Just $ Span a b
  | otherwise = Nothing

-- NOTE there's nothing that swaps the start/stop positions of the span
-- that is intentional, as I think it could lead to unexpected bugs

instance Show Span where
  showsPrec p spn rest
    = showsPrec p spn.start
    $ ("--" ++)
    $ showsPrec p spn.end
    $ rest
instance Read Span where
  readPrec = do
    start <- readPrec @Pos
    Read.get >>= \case { '-' -> pure (); _ -> pfail }
    Read.get >>= \case { '-' -> pure (); _ -> pfail }
    end <- readPrec @Pos
    maybe pfail pure $ mkSpan start end

--------------
---- TODO ----
--------------

-- TODO a type that adds a filepath (or stdin, or whatever) element to a location-y thing

-- TODO a type for describing locations even when macros are involved
-- data Loc = Loc (NonEmpty Span)
--   deriving (Show)

