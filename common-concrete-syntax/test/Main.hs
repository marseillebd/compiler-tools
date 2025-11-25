module Main (main) where

import Data.Text (Text)
import Language.CCS.Token.Raw (tokenize)
import System.FilePath ((</>), (<.>))
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Tokenizer"
    [ golden "test all legal raw tokens at once" "allTokens" $ \input -> do
      output <- tokenize input
      -- TODO test numbers, signed numbers, non-decimal, floating
      -- TODO test strings, ml strings
      pure $ T.unlines $ T.pack . show <$> output
    , golden "empty input file has no tokens" "noTokens" $ \input -> do
      output <- tokenize input
      pure $ T.unlines $ T.pack . show <$> output
    ]
  ]

golden ::
     String -- ^ test name
  -> FilePath -- ^ basename of a file
  -> (Text -> IO Text) -- ^ transform the input file to an output file
  -> TestTree
golden name file f = goldenVsFile name gfile ofile go
  where
  go = do
    input <- T.readFile ifile
    output <- f input
    T.writeFile ofile output
  ifile = "test" </> "cases" </> file <.> "input"
  ofile = "test" </> "cases" </> file <.> "output"
  gfile = "test" </> "cases" </> file <.> "golden"
