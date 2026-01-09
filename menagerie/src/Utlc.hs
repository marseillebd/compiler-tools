{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Utlc
  ( main
  , Ast(..), Expr(..)
  ) where

import Prelude hiding (id, (.), fail)
import Language.CCS

import Control.Monad (forM_)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Language.Nanopass (deflang)
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

main :: FilePath -> IO ()
main filepath = do
  ast <- readSource filepath
  print ast


[deflang|
(Ast
  (Expr
    (Var Span Text)
    (Fun Span Text Expr)
    (App Span Expr Expr)
  )
)
|]

deriving instance Show Expr

------ Reader ------

readSource :: FilePath -> IO Expr
readSource filepath = do
  bytes <- LBS.readFile filepath
  (lexErrs, (parseErrs, cst_m)) <- execErr $ csts bytes
  case lexErrs of
    [] -> pure ()
    errs -> print errs >> exitFailure
  case parseErrs of
    [] -> pure ()
    errs -> print errs >> exitFailure
  cst <- case cst_m of
    Just [it] -> pure it
    Just _ -> putStrLn "expecting exactly one expression" >> exitFailure
    Nothing -> putStrLn "SYNTAX ERROR" >> exitFailure
  ast <- case run recogExpr cst of
    Right ast -> pure ast
    Left errs -> do
      forM_ errs $ \err -> do
        putStrLn . T.unpack $ T.pack (show err.context) <> ": " <> err.message
      exitFailure
  pure ast

recogExpr :: CST ~> Expr
recogExpr = recogVar <+> recogFun <+> recogApp <+> recogParen

recogParen :: CST ~> Expr
recogParen = recogExpr <<< requireR "expecting parenthesized expression" <<< parens

requireR :: Text -> Maybe a ~> a
requireR msg = proc m -> case m of
  Just x -> returnA -< x
  Nothing -> fail msg -< ()

recogVar :: CST ~> Expr
recogVar = proc cst -> do
  x <- recogSymbol -< cst
  returnA -< Var cst.span x

recogFun :: CST ~> Expr
recogFun = proc cst -> do
  a :| bodyCst <- spaced -< cst
  kwCst :| chainedCsts <- chained -< a
  () <- recogKw "fn" -< kwCst
  paramsCst <- case chainedCsts of
    [it] -> parenList -< it
    _ -> fail "expecting parameter list" -< ()
  x <- case paramsCst of
    [paramCst] -> recogSymbol -< paramCst
    _ -> fail "expecting one parameter" -< ()
  expr <- case bodyCst of
    [it] -> recogExpr -< it
    _ -> fail "expecting function body" -< ()
  returnA -< Fun cst.span x expr

recogApp :: CST ~> Expr
recogApp = proc cst -> do
  a :| bs <- chained -< cst
  b <- case bs of
    [b] -> returnA -< b
    _ -> fail "expecting function call" -< ()
  fun <- recogExpr -< a
  arg <- recogExpr -< b
  returnA -< App cst.span fun arg



--- Primitive Recognizers ---

recogKw :: Text -> CST ~> ()
recogKw needle = proc cst -> do
  haystack <- symbol <<< atom -< cst
  if needle == haystack
  then returnA -< ()
  else fail ("expecting `" <> needle <> "`") -< ()

recogSymbol :: CST ~> Text
recogSymbol = symbol <<< atom

----------------------------
------ Error Handling ------
----------------------------

newtype Err a = Err { runErr :: IORef [Text] -> IO a }
execErr :: Err a -> IO ([Text], a)
execErr action = do
  ref <- newIORef []
  out <- runErr action ref
  err <- readIORef ref
  pure (reverse err, out)
instance Functor Err where
  fmap f getX = Err $ \env -> f <$> runErr getX env
instance Applicative Err where
  pure x = Err $ \_ -> pure x
  getF <*> getX = Err $ \env -> do
    f <- runErr getF env
    x <- runErr getX env
    pure $ f x
instance Monad Err where
  getX >>= k = Err $ \env -> do
    x <- runErr getX env
    runErr (k x) env
addErr :: String -> Err ()
addErr msg = Err $ \env -> modifyIORef env $ (T.pack msg :)
instance DeleteComment Err where
  deleteComment _ = pure ()
instance RaiseIllegalBytes Err where
  raiseIllegalBytesOrChars txt = addErr $ concat
    [ "IllegalBytesOrChars: ", show txt ]
instance WhitespaceError Err where
  raiseTrailingWhitespace l = addErr $ concat
    [ "TrailingWhitespace: ", show l ]
  raiseInconsistentNewlines err = addErr $ concat
    [ "InconosistentNewlines: "
    , show err
    ]
  raiseNoNlAtEof l = addErr $ concat
    [ "NoNlAtEof: ", show l ]
instance MalformedPunctuation Err where
  raiseTooManyDots l = addErr $ concat
    [ "TooManyDots: ", show l ]
  raiseTooManyColons l = addErr $ concat
    [ "TooManyColons: ", show l ]
instance MalformedNumber Err where
  raiseExpectingFractionalDigits l = addErr $ concat
    [ "ExpectingFractionalDigits: ", show l
    ]
  raiseExpectingExponent l = addErr $ concat
    [ "ExpectingExponent: ", show l ]
  raiseUnexpectedExponent l = addErr $ concat
    [ "UnexpectedExponent: ", show l ]
instance MalformedString Err where
  raiseExpectingCloseQuote l = addErr $ concat
    [ "ExpectingCloseQuote: ", show l ]
instance MalformedIndentation Err where
  raiseUnexpectedIndent l = addErr $ concat
    [ "UnexpectedIndent: ", show l ]
  raiseInsufficientIndentation l = addErr $ concat
    [ "InsufficientIndentation: ", show l ]
  raiseLeadingWhitespace l = addErr $ concat
    [ "LeadingWhitespace: ", show l ]
instance SandhiError Err where
  raiseCrammedTokens l = addErr $ concat
    [ "CrammedTokens: ", show l]
  raiseExpectedWhitespace l = addErr $ concat
    [ "ExpectedWhitespace: ", show l]
  raiseBareIndent l = addErr $ concat
    [ "BareIndent: ", show l]
  raiseUnexpectedWhitespace l = addErr $ concat
    [ "UnexpectedWhitespace: ", show l]
  raiseUnexpectedDot l = addErr $ concat
    [ "UnexpectedDot: ", show l]
  raiseUnexpectedColon l = addErr $ concat
    [ "UnexpectedColon: ", show l]
  raiseUnexpectedBackslash l = addErr $ concat
    [ "UnexpectedBackslash: ", show l]
