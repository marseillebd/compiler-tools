{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Data.Function ((&))
import Data.Text (Text)
import System.FilePath ((</>), (<.>))
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Language.CCS.Lexer.Morpheme.NoiseReduction (DeleteComment(..), RaiseIllegalBytes(..), WhitespaceError(..))
import Language.CCS.Lexer.Morpheme.AssembleNumbers (MalformedNumber(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.CCS.Lexer.Morpheme.AssembleNumbers as LexAN
import qualified Language.CCS.Lexer.Morpheme.NoiseReduction as LexNR
import qualified Language.CCS.Lexer.Pipeline as Morpheme
import qualified Streaming.Prelude as S

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Tokenizer (morphemes)"
    [ golden "test all legal raw tokens at once" "allTokens" $ \input -> do
      let output = Morpheme.pipeline input
      pure $ T.unlines $ T.pack . show <$> output
    , golden "empty input file has no tokens" "noTokens" $ \input -> do
      let output = Morpheme.pipeline input
      pure $ T.unlines $ T.pack . show <$> output
    ]
  , testGroup "Tokenizer (lexemes)"
    [ golden "smoke test all lexemes" "allLexemes" $ \input -> do
      output <- input
            & Morpheme.pipeline
            & S.each
            & LexNR.pipeline
            & LexAN.assemble
            & S.toList
            & runErr
      pure $ T.unlines $ T.pack . show <$> S.fst' output
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

newtype Err a = Err { runErr :: IO a }
  deriving (Functor, Applicative, Monad)
instance DeleteComment Err where
  deleteComment _ _ = pure ()
instance RaiseIllegalBytes Err where
  raiseIllegalBytesOrChars l txt = Err $ putStrLn $ concat
    [ "IllegalBytesOrChars: "
    , show l, " "
    , show txt
    ]
instance WhitespaceError Err where
  raiseTrailingWhitespace l = Err $ putStrLn $ concat
    [ "TrailingWhitespace: ", show l
    ]
  raiseInconsistentNewlines err = Err $ putStrLn $ concat
    [ "InconosistentNewlines: "
    , show err
    ]
  raiseNoNlAtEof l = Err $ putStrLn $ concat
    [ "NoNlAtEof: ", show l ]
instance MalformedNumber Err where
  raiseExpectingIntegerDigits l = Err $ putStrLn $ concat
    [ "ExpectingIntegerDigits: ", show l
    ]
  raiseNegativeExponentForInteger l = Err $ putStrLn $ concat
    [ "NegativeExponentForInteger: ", show l ]
  raiseExpectingExponent l = Err $ putStrLn $ concat
    [ "ExpectingExponent: ", show l ]
  raiseUnexpectedSign l = Err $ putStrLn $ concat
    [ "UnexpectedSign: ", show l ]
  raiseUnexpectedPower l = Err $ putStrLn $ concat
    [ "UnexpectedPower: ", show l ]
