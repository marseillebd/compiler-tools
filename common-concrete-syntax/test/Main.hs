{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function ((&))
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Data.Text (Text)
import Language.CCS.Error (ReaderHooks(..))
import Language.CCS.Lexer (tokens, lexemes, lexemesToCsts)
import System.FilePath ((</>), (<.>))
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Text.Pretty.Simple (pShowNoColor)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Tokenizer"
    [ golden "test all legal raw tokens at once" "allTokens" $ \input -> do
      let output = tokens input
      pure $ T.unlines $ T.pack . show <$> output
    , golden "smoke test all lexemes as raw tokens" "allRawLexemes" $ \input -> do
      let output = tokens input
      pure $ T.unlines $ T.pack . show <$> output
    , golden "empty input file has no tokens" "noTokens" $ \input -> do
      let output = tokens input
      pure $ T.unlines $ T.pack . show <$> output
    ]
  , testGroup "Lexer"
    [ golden "smoke test all lexemes" "allLexemes" $ \input -> do
      (err, out) <- input
            & lexemes
            & execErr
      pure $ T.concat
        [ err
        , T.unlines $ T.pack . show <$> out
        ]
    ]
  , testGroup "Parser"
    [ golden "smoke test all syntax tree constructs" "allTrees" $ \input -> do
      -- let coverage = tokens input
      (lexErr, toks) <- execErr $ lexemes input
      (parseErrs, out_m) <- execErr $ lexemesToCsts toks
      pure $ T.concat
        -- [ T.unlines $ T.pack . show <$> coverage
        -- , "\n------------------\n"
        -- , lexErr
        -- , "\n"
        -- , T.unlines $ T.pack . show <$> toks
        -- , "\n------------------\n"
        [ parseErrs
        , "\n"
        , T.unlines $ (LT.toStrict . pShowNoColor) <$> out_m
        ]
    ]
  ]

golden ::
     String -- ^ test name
  -> FilePath -- ^ basename of a file
  -> (LBS.ByteString -> IO Text) -- ^ transform the input file to an output file
  -> TestTree
golden name file f = goldenVsFile name gfile ofile go
  where
  go = do
    input <- LBS.readFile ifile
    output <- f input
    T.writeFile ofile output
  ifile = "test" </> "cases" </> file <.> "input"
  ofile = "test" </> "cases" </> file <.> "output"
  gfile = "test" </> "cases" </> file <.> "golden"

newtype Err a = Err { runErr :: IORef Text -> IO a }
execErr :: Err a -> IO (Text, a)
execErr action = do
  ref <- newIORef ""
  out <- runErr action ref
  err <- readIORef ref
  pure (err, out)
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
addErr msg = Err $ \env -> modifyIORef env $ (<> (T.pack msg <> "\n"))
instance ReaderHooks Err where
  ignore _ = pure ()
  styleNote = addErr . show
  recoverableError err = addErr $ show err
