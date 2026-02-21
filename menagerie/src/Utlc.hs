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

import Prelude hiding (fail, last, lines)
import Language.CCS
import Language.CCS.Recognize

import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Language.Nanopass (deflang)
import System.Exit (exitFailure)

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as LBS

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
recogExpr = recogVar <> recogFun <> recogApp <> recogParen recogExpr

recogParen :: (CST ~> Expr) -> (CST ~> Expr)
recogParen action (Enclose _ Round (Just x)) = action x
recogParen _ other = fail $ Expected "parentheses" (Just other)

recogVar :: CST ~> Expr
recogVar cst = Var cst.span <$> recogId cst

recogFun :: CST ~> Expr
recogFun cst = do
  -- recognize keywoord
  (afterKw, bodyTree) <- flip test cst $ \case
    List _ Space (it :| bodyTree)
      | List _ Chain (kw:|afterKw) <- it
      , Atom _ (Symbol "fn") <- kw
      -> Right ((kw.span.end, afterKw), (it.span.end, bodyTree))
    _ -> Left $ Expected "function literal `fn(...) ...`" (Just cst)
  -- parameter list
  (x, afterFnHead) <- parse afterKw $ do
    params <- satisfies parenList
              >>= onLeftM (\loc -> fail $ NoParamsAfterFn loc)
    param <- case params of
      it:more -> do
        case more of
          [] -> pure ()
          other:_ -> report $ Expected "at most one parameter" (Just other)
        pure it
      [] -> fail $ Expected "non-empty parameter list" Nothing
    recogId param
  headM afterFnHead $ \other ->
    report $ Unexpected (Just "after parameter list") other
  -- function body
  (expr, afterBody) <- parse bodyTree $ do
    pop >>= onLeftM (\loc -> fail $ NoFunctionBody (Left loc))
        >>= recogExpr
  headM afterBody $ \other ->
    report $ Unexpected (Just "after function body") other
  pure $ Fun cst.span x expr

recogApp :: CST ~> Expr
recogApp cst = do -- TODO try writing it a little nicer
  (f, x) <- case cst of
    List _ Chain (f:|xs)
      | [Enclose _ Round (Just x)] <- xs
      -> pure (f, x)
    _ -> fail $ Expected "function call" (Just cst)
  App cst.span <$> recogExpr f <*> recogExpr x

--- Primitive Recognizers ---

-- recogKw :: Text -> CST ~> ()
-- recogKw needle = arrowR $ \cst -> do
--   let msg = Expected ("`" <> needle <> "`") (Just cst)
--   kwId <- monadR (labelR msg $ recogKwOrId) cst
--   case kwId of
--     Left hay | hay == needle -> pure ()
--     Left _ -> monadR raiseR msg
--     Right _ -> monadR raiseR msg

recogId :: CST ~> Text
recogId cst = recogKwOrId cst >>= \case
    Right x -> pure x
    Left _ -> fail $ Expected "identifier" (Just cst)

recogKwOrId :: CST ~> Either Text Text
recogKwOrId = test $ \case
  Atom _ (Symbol x) -> Right $
    if x `elem` kws
    then Left x else Right x
  other -> Left $ Expected "symbol" (Just other)
  where
  kws =
    [ "fn"
    ]

type a ~> b = a -> Recog RecogError b

------ FIXME cst-only recognizers ------
--move elsewhere

-- parseNE :: Recog e CST b -> Recog e (NonEmpty CST) b
-- parseNE action = proc xs -> do
--   (b, _) <- parseR (.span) action -< ((NE.head xs).span.start, xs)
--   returnA -< b

-- satRL :: (Pos -> e) -> Recog e tok r -> RecogList e tok r
-- satRL msg action = lookRL $ proc look -> do
--   case look of
--     Right next -> action -< next
--     Left pos -> raiseR -< msg pos

-- endRL :: (tok -> e) -> RecogList e tok ()
-- endRL msg = lookRL $ satR $ \case
--   Right next -> Left $ msg next
--   Left pos -> Right ()

-- enclosedR :: Encloser -> (CST ~> Maybe CST)
-- enclosedR brak = satR $ \case
--   Enclose _ brak' inner | brak == brak' -> Right inner
--   other -> Left $ Expected brakDescr (Just other)
--   where
--   brakDescr = case brak of
--     Round -> "parentheses"
--     Square -> "square brackets"
--     Curly -> "curly brackets"

-- separatedRL :: Separator -> RecogList RecogError CST b -> (CST ~> b)
-- separatedRL sep action = proc tree -> do
--   case tree of
--     List _ sep' xs | sep == sep' -> parseNE action -< xs
--     other -> raiseR -< Expected sepDescr (Just other)
--   where
--   sepDescr = case sep of
--     Semicolon -> "semicolon-separated list"
--     Comma -> "comma-separated list"
--     Space -> "cst list" -- TODO how do I describe this?
--     Chain -> "access/call chain"
--     Qualify -> "qualified atom"

-- parseR_ getP action = fst <$> parseR getP action

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
parenList :: CST -> Recog e (Either CST [CST])
parenList = pure . \case
  Enclose _ Round Nothing -> Right []
  Enclose _ Round (Just inner)
    | List _ Comma xs <- inner -> Right $ NE.toList xs
    | Block _ lines <- inner -> do
      let adapt (List _ Comma xs) = NE.toList xs
          adapt x = [x]
      Right $ concat $ adapt <$> lines
    | otherwise -> Right [inner]
  other -> Left other

satisfies :: (Foldable f) => (CST -> Recog e (f a)) -> Recog e (Either Expect a)
satisfies action = pop >>= \case
  Right cst -> do
    results <- action cst
    case toList results of
      x:_ -> pure (Right x)
      [] -> pure $ Left (Right cst)
  Left loc -> pure $ Left (Left loc)

type Expect = Either Span CST

-- feed :: (Monad m) => m a -> (a -> m b) -> m b

----------------------------
------ Error Handling ------
----------------------------

data RecogError
  = Expected { expectDescr :: Text, foundE :: Maybe CST }
  | Unexpected { contextDescr :: Maybe Text, foundU :: CST }
  | NoParamsAfterFn (Either Span CST)
  | NoFunctionBody (Either Span CST)
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
