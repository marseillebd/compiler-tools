{-# LANGUAGE OverloadedStrings #-}

module Language.Text
  ( SrcText
  , fromPos
  , null
  , Parse
  , runParse
  , execParse
  , withSpan
  , asSrc
  , sat
  , look
  , tryN
  , consumed
  , atEnd
  , take
  , takeWhile
  , takeWhile1
  , takeAll
  , takePrefix
  , branch
) where

import Prelude hiding (take, takeWhile, null)

import Control.Applicative (Alternative(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Records (HasField(..))
import Language.Location (Pos, incCol, incLine, Span, mkSpan)

import qualified Data.Text as T

data SrcText = SrcText
  { _span :: Span
  , _text :: Text
  }
instance Show SrcText where
  show src = concat
    [ show src.span, " ", show src.text ]

fromPos :: Pos -> Text -> SrcText
fromPos p text = SrcText
  { _span = fromJust $ mkSpan p (p `advText` text)
  , _text = text
  }

null :: SrcText -> Bool
null = T.null . _text

advChar :: Pos -> Char -> Pos
advChar p '\n' = incLine p
advChar p _ = incCol p

advText :: Pos -> Text -> Pos
advText p0 = T.foldl' advChar p0

instance HasField "span" SrcText Span where getField = _span
instance HasField "text" SrcText Text where getField = _text

data St = St
  { start :: !Pos
  , taken :: !Text
  , mid :: !Pos
  , rest :: !Text
  , end :: !Pos
  }

data Parse a = P { unP :: St -> Maybe (St, a) }

instance Functor Parse where
  fmap f getX = P $ \st -> do
    (st', x) <- unP getX st
    pure (st', f x)
instance Applicative Parse where
  pure x = P $ \st -> Just (st, x)
  getF <*> getX = P $ \st -> do
    (st', f) <- unP getF st
    (st'', x) <- unP getX st'
    pure (st'', f x)
instance Alternative Parse where
  empty = P $ const Nothing
  getA <|> getB = P $ \st -> do
    let a_m = unP getA st
        b_m = unP getB st
    a_m <|> b_m
instance Monad Parse where
  getX >>= k = P $ \st -> do
    (st', x) <- unP getX st
    unP (k x) st'

runParse :: SrcText -> Parse a -> Maybe (SrcText, a, SrcText)
runParse src action = (unP action) st0 >>= \(st, x) -> do
    locA <- mkSpan st.start st.mid
    locB <- mkSpan st.mid st.end
    pure (SrcText locA st.taken, x, SrcText locB st.rest)
  where
  st0 = St
    { start = src.span.start
    , taken = ""
    , mid = src.span.start
    , rest = src.text
    , end = src.span.end
    }

execParse :: SrcText -> Parse a -> Maybe (SrcText, SrcText)
execParse src p = do
  (yes, _, remain) <- runParse src p
  pure (yes, remain)

withSpan :: Parse a -> Parse (Span, a)
withSpan p = P $ \st -> do
  (st', txt) <- unP p st
  let loc = fromJust $ mkSpan st.mid st'.mid
  pure $ (st', (loc, txt))

asSrc :: Parse Text -> Parse SrcText
asSrc p = do
  (loc, txt) <- withSpan p
  pure $ SrcText loc txt

-- ^ take one character from the input,
-- fail if it does not satisfy the predicate or if the end is reached
sat :: (Char -> Bool) -> Parse Char
sat p = P $ \st -> case st.rest of
  c T.:< r | p c -> Just (st
    { taken = st.taken T.:> c
    , mid = st.mid `advChar` c
    , rest = r
    }, c)
  _ -> Nothing

-- ^ lookahead at the next few characters without parsing
look :: Int -> Parse Text
look n = P $ \st -> Just (st, T.take n st.rest)

tryN :: Int -> (Text -> Either a a) -> Parse a
tryN n f = do
  text <- look n
  case f text of
    Left def -> pure def
    Right ok -> take n >> pure ok

consumed :: Parse SrcText
consumed = P $ \st ->
  let x = SrcText (fromJust $ mkSpan st.start st.mid) st.taken
   in Just (st, x)

atEnd :: Parse Bool
atEnd = P $ \st -> case st.rest of
  T.Empty -> pure (st, True)
  _ -> pure (st, False)

take :: Int -> Parse Text
take n = P $ \st -> do
  let (yes, no) = T.splitAt n st.rest
  pure (st
    { taken = st.taken <> yes
    , mid = st.mid `advText` yes
    , rest = no
    }, yes)

takeWhile :: (Char -> Bool) -> Parse Text
takeWhile p = takeWhile1 p <|> pure ""

takeWhile1 :: (Char -> Bool) -> Parse Text
takeWhile1 p = P $ \st -> do
  let (yes, no) = T.span p st.rest
  if T.null yes then Nothing else
    pure $ (st
      { taken = st.taken <> yes
      , mid = st.mid `advText` yes
      , rest = no
      }, yes)

takeAll :: Parse Text
takeAll = P $ \st -> do
  pure $ (st
    { taken = st.taken <> st.rest
    , mid = st.end
    , rest = ""
    }, st.rest)

takePrefix :: Text -> Parse Text
takePrefix needle = P $ \st -> do
  suffix <- T.stripPrefix needle st.rest
  pure (st
    { taken = st.taken <> needle
    , mid = st.mid `advText` needle
    , rest = suffix
    }, needle)

-- ^ given a list of predicate-consequent pairs, once one predicate succeeds, parse its consequent.
-- once a consequent has been chosen, do not backtrack
branch :: [(Parse a, a -> Parse b)] -> Parse b
branch arcs = P $ \st ->
  let loop [] = empty
      loop ((p, c) : as) = case unP p st of
        Just (st', x) -> unP (c x) st'
        Nothing -> loop as
   in loop arcs
