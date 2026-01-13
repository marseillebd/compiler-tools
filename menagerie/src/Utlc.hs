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

import Prelude hiding (id, (.), fail, last)
import Language.CCS hiding (type (~>), Expecting, parenList)
import Language.CCS.Recognize.New (Recog, runRecog, satR, raiseR, monadR, arrowR, labelR, Result(..), RecogList, parseR, lookRL, posRL, RecogMonad, feedR, reportR, restRL, monadRL)

import Control.Monad (forM_)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Language.Nanopass (deflang)
import System.Exit (exitFailure)

import qualified Data.List.NonEmpty as NE
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
  trees <- loop readerResult
  cst <- case trees of
    [it] -> pure it
    _ -> putStrLn "expecting exactly one expression" >> exitFailure
  ast <- case runRecog recogExpr cst of
    Result [] (Right ast) -> pure ast
    Result errs last -> do
      forM_ errs $ \err -> do
        print err
      case last of
        Left err -> print err
        Right _ -> pure ()
      exitFailure
  pure ast

recogExpr :: CST ~> Expr
recogExpr = recogVar <> recogFun -- <> recogApp <> recogParen -- FIXME

-- recogParen :: CST ~> Expr
-- recogParen = recogExpr <<< requireR "expecting parenthesized expression" <<< parens

-- requireR :: Text -> Maybe a ~> a
-- requireR msg = proc m -> case m of
--   Just x -> returnA -< x
--   Nothing -> fail msg -< ()

recogVar :: CST ~> Expr
recogVar = proc cst -> do
  x <- recogId -< cst
  returnA -< Var cst.span x

recogFun :: CST ~> Expr
recogFun = arrowR $ \cst -> do
  (afterKw, bodyTree) <- feedR cst $ satR $ \case
    List _ Space (it :| bodyTree)
      | List _ Chain (kw:|afterKw) <- it
      , Atom kwLoc (Symbol "fn") <- kw
      -> Right ((kwLoc.end, afterKw), bodyTree)
    _ -> Left $ Expecting "function literal `fn(...) ...`" (Just cst)
  x <- feedR afterKw $ parseR_ (.span) $ do
    let noParams = Expecting "parameter list"
    params <- satRL (const $ noParams Nothing) $
      parenListR (noParams . Just)
    param <- case params of
      [it] -> pure it
      [] -> monadRL raiseR $ Expecting "non-empty parameter list" Nothing
      _:other:_ -> monadRL raiseR (Expecting "at most one parameter" (Just other))
    x <- monadRL recogId param
    endRL (Unexpected (Just "after parameter list"))
    pure x
  expr <- case bodyTree of
    it:rest -> do
      expr <- monadR recogExpr it
      case rest of
        [] -> pure ()
        _ -> forM_ rest $ \other ->
          monadR reportR $ Unexpected (Just "after function body") other
      pure expr
    [] -> monadR raiseR $ Expecting "function body" Nothing
  pure $ Fun cst.span x expr

-- recogApp :: CST ~> Expr
-- recogApp = proc cst -> do
--   a :| bs <- chained -< cst
--   b <- case bs of
--     [b] -> returnA -< b
--     _ -> fail "expecting function call" -< ()
--   fun <- recogExpr -< a
--   arg <- recogExpr -< b
--   returnA -< App cst.span fun arg



--- Primitive Recognizers ---

recogKw :: Text -> CST ~> ()
recogKw needle = arrowR $ \cst -> do
  let msg = Expecting ("`" <> needle <> "`") (Just cst)
  kwId <- monadR (labelR msg $ recogKwOrId) cst
  case kwId of
    Left hay | hay == needle -> pure ()
    Left _ -> monadR raiseR msg
    Right _ -> monadR raiseR msg

recogId :: CST ~> Text
recogId = arrowR $ \cst ->
  monadR recogKwOrId cst >>= \case
    Right x -> pure x
    Left _ -> monadR raiseR $ Expecting "identifier" (Just cst)

recogKwOrId :: CST ~> Either Text Text
recogKwOrId = satR $ \case
  Atom _ (Symbol x) -> Right $
    if x `elem` kws
    then Left x else Right x
  other -> Left $ Expecting "symbol" (Just other)
  where
  kws =
    [ "fn"
    ]

type (~>) = Recog RecogError

------ FIXME cst-only recognizers ------
--move elsewhere

parseNE :: RecogList e CST b -> Recog e (NonEmpty CST) b
parseNE action = proc xs -> do
  (b, _) <- parseR (.span) action -< ((NE.head xs).span.start, xs)
  returnA -< b

satRL :: (Pos -> e) -> Recog e tok r -> RecogList e tok r
satRL msg action = lookRL $ proc look -> do
  case look of
    Right next -> action -< next
    Left pos -> raiseR -< msg pos

endRL :: (tok -> e) -> RecogList e tok ()
endRL msg = lookRL $ satR $ \case
  Right next -> Left $ msg next
  Left pos -> Right ()

enclosedR :: Encloser -> (CST ~> Maybe CST)
enclosedR brak = satR $ \case
  Enclose _ brak' inner | brak == brak' -> Right inner
  other -> Left $ Expecting brakDescr (Just other)
  where
  brakDescr = case brak of
    Round -> "parentheses"
    Square -> "square brackets"
    Curly -> "curly brackets"

separatedRL :: Separator -> RecogList RecogError CST b -> (CST ~> b)
separatedRL sep action = proc tree -> do
  case tree of
    List _ sep' xs | sep == sep' -> parseNE action -< xs
    other -> raiseR -< Expecting sepDescr (Just other)
  where
  sepDescr = case sep of
    Semicolon -> "semicolon-separated list"
    Comma -> "comma-separated list"
    Space -> "cst list" -- TODO how do I describe this?
    Chain -> "access/call chain"
    Qualify -> "qualified atom"

parseR_ getP action = fst <$> parseR getP action

-- | A list of csts, separated by commas, enclosed in parenthesis.
-- Also allows for parenthesized indentation, where each line is a possibly comma-separated list of CSTs.
--
-- For example, this parser will accept and produce identical outputs for all of the following trees:
-- @
-- (1, 2, 3)
--
-- (
--   1
--   2
--   3
-- )
--
-- (
--   1, 2
--   3
--  )
-- @
parenListR :: (CST -> e) -> Recog e CST [CST]
parenListR onError = satR $ \case
  Enclose _ Round Nothing -> Right []
  Enclose _ Round (Just inner)
    | List _ Comma xs <- inner -> Right $ NE.toList xs
    | Block _ lines <- inner -> do
      let adapt (List _ Comma xs) = NE.toList xs
          adapt x = [x]
      Right $ concat $ adapt <$> lines
    | otherwise -> Right [inner]
  other -> Left $ onError other

----------------------------
------ Error Handling ------
----------------------------

data RecogError
  = Expecting { expectDescr :: Text, foundE :: Maybe CST }
  | Unexpected { contextDescr :: Maybe Text, foundU :: CST }
  deriving (Show)

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
