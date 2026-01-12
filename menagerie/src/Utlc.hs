{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

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
  readerResult <- execErr $ csts bytes
  let loop (Done (Right out)) = pure out
      loop (Done (Left err)) = print err >> exitFailure
      loop (Next err rest) = print err >> loop rest
  csts <- loop readerResult
  cst <- case csts of
    [it] -> pure it
    _ -> putStrLn "expecting exactly one expression" >> exitFailure
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

newtype Err a = Err { runErr :: IORef [ReaderError] -> IO (Either ReaderError a) }
execErr :: Err a -> IO (Linear ReaderError (Either ReaderError a))
execErr action = do
  ref <- newIORef []
  out <- runErr action ref
  err <- readIORef ref
  pure $ foldl (flip Next) (Done out) err
instance Functor Err where
  fmap f getX = Err $ \env -> (fmap . fmap) f $ runErr getX env
instance Applicative Err where
  pure x = Err $ \_ -> pure (Right x)
  getF <*> getX = Err $ \env -> do
    runErr getF env >>= \case
      Right f -> runErr getX env >>= \case
        Right x -> pure (Right $ f x)
        Left e -> pure (Left e)
      Left e -> pure (Left e)
instance Monad Err where
  getX >>= k = Err $ \env -> do
    runErr getX env >>= \case
      Right x -> runErr (k x) env
      Left e -> pure $ Left e

instance ReaderHooks Err where
  ignore _ = pure ()
  styleNote _ = pure ()
  recoverableError err = Err $ \env -> Right <$> modifyIORef env (err:)
  fatalError err = Err $ \_ -> pure $ Left err

data Linear a z
  = Next a (Linear a z)
  | Done z
