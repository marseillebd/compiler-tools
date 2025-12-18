{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function ((&))
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)
import Data.Text (Text)
import System.FilePath ((</>), (<.>))
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
-- import Language.CCS.Lexer.NoiseReduction (DeleteComment(..), RaiseIllegalBytes(..), WhitespaceError(..))
-- import Language.CCS.Lexer.Assemble.Numbers (MalformedNumber(..))
-- import Language.CCS.Lexer.Assemble.Strings (MalformedString(..))
-- import Language.CCS.Lexer.Sandhi.Indentation (MalformedIndentation(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Language.CCS.Lexer.Assemble.Numbers as LexAN
-- import qualified Language.CCS.Lexer.Assemble.Strings as LexAS
-- import qualified Language.CCS.Lexer.NoiseReduction as LexNR
import qualified Language.CCS.Lexer.Pipeline as Morpheme
-- import qualified Language.CCS.Lexer.Sandhi.Indentation as LexSI
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
  -- , testGroup "Tokenizer (lexemes)"
  --   [ golden "smoke test all lexemes" "allLexemes" $ \input -> do
  --     (err, out) <- input
  --           & Morpheme.pipeline
  --           & S.each
  --           & LexNR.pipeline
  --           & LexAN.assemble
  --           & LexAS.assemble
  --           & LexSI.process
  --           & S.toList
  --           & execErr
  --     pure $ T.concat
  --       [ err
  --       , T.unlines $ T.pack . show <$> S.fst' out
  --       ]
  --   ]
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
-- instance DeleteComment Err where
--   deleteComment _ _ = pure ()
-- instance RaiseIllegalBytes Err where
--   raiseIllegalBytesOrChars l txt = addErr $ concat
--     [ "IllegalBytesOrChars: "
--     , show l, " "
--     , show txt
--     ]
-- instance WhitespaceError Err where
--   raiseTrailingWhitespace l = addErr $ concat
--     [ "TrailingWhitespace: ", show l
--     ]
--   raiseInconsistentNewlines err = addErr $ concat
--     [ "InconosistentNewlines: "
--     , show err
--     ]
--   raiseNoNlAtEof l = addErr $ concat
--     [ "NoNlAtEof: ", show l ]
-- instance MalformedNumber Err where
--   raiseExpectingIntegerDigits l = addErr $ concat
--     [ "ExpectingIntegerDigits: ", show l
--     ]
--   raiseNegativeExponentForInteger l = addErr $ concat
--     [ "NegativeExponentForInteger: ", show l ]
--   raiseExpectingExponent l = addErr $ concat
--     [ "ExpectingExponent: ", show l ]
--   raiseUnexpectedSign l = addErr $ concat
--     [ "UnexpectedSign: ", show l ]
--   raiseUnexpectedPower l = addErr $ concat
--     [ "UnexpectedPower: ", show l ]
-- instance MalformedString Err where
--   raiseExpectingCloseQuote l = addErr $ concat
--     [ "ExpectingCloseQuote: ", show l ]
-- instance MalformedIndentation Err where
--   raiseUnexpectedIndent l = addErr $ concat
--     [ "UnexpectedIndent: ", show l ]
--   raiseInsufficientIndentation l = addErr $ concat
--     [ "InsufficientIndentation: ", show l ]
--   raiseBadWhitespaceBeforeMultiLineDelimiter l expected = addErr $ concat
--     [ "BadWhitespaceBeforeMultiLineDelimiter: ", show l, " expected ", show expected ]
--   raiseLeadingWhitespace l = addErr $ concat
--     [ "LeadingWhitespace: ", show l ]
