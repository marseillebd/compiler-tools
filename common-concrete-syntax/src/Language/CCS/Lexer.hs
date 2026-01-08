module Language.CCS.Lexer
  ( tokensFrom
  , tokens
  , tokensToLexemes
  , lexemesFrom
  , lexemes
  , lexemesToCstsFrom
  , lexemesToCsts
  , cstsFrom
  , csts
  , LexerError
  , RaiseIllegalBytes(..)
  , DeleteComment(..)
  , WhitespaceError(..)
  , MalformedPunctuation(..)
  , MalformedNumber(..)
  , MalformedString(..)
  , MalformedIndentation(..)
  , SandhiError(..)
  , ParseError(..)
  ) where

import Prelude hiding (lex, lines, exp)

import Data.Function ((&))
import Language.Location (Pos, startPos)

import qualified Data.ByteString.Lazy as LBS
import qualified Streaming.Prelude as S

--------------------------------
------ The Lexer Pipeline ------
--------------------------------

import qualified Language.CCS.Lexer.Decode as Decode
import qualified Language.CCS.Lexer.Cover as Cover
import qualified Language.CCS.Lexer.NoiseReduction as NoiseReduction
import qualified Language.CCS.Lexer.Assemble as Assemble
import qualified Language.CCS.Lexer.Indentation as Indentation
import qualified Language.CCS.Lexer.Sandhi as Sandhi
import qualified Language.CCS.Parser as Parser

import Language.CCS.Lexer.NoiseReduction (RaiseIllegalBytes, DeleteComment, WhitespaceError)
import Language.CCS.Lexer.Assemble (MalformedPunctuation, MalformedNumber, MalformedString)
import Language.CCS.Lexer.Indentation (MalformedIndentation)
import Language.CCS.Lexer.Sandhi (SandhiError)
import Language.CCS.Parser (ParseError(..))

type LexerError m =
  ( RaiseIllegalBytes m
  , DeleteComment m
  , WhitespaceError m
  , MalformedPunctuation m
  , MalformedNumber m
  , MalformedString m
  , MalformedIndentation m
  , SandhiError m
  )

tokensFrom :: Pos -> LBS.ByteString -> [Cover.Token]
tokensFrom pos0 bytes = bytes
  & Decode.decode
  & Decode.linesFrom pos0
  & Cover.lexLines

tokens :: LBS.ByteString -> [Cover.Token]
tokens = tokensFrom startPos

tokensToLexemes :: LexerError m => [Cover.Token] -> m [Sandhi.Token]
tokensToLexemes inp = inp
  & S.each
  & NoiseReduction.pipeline
  & Assemble.assemble
  & Indentation.process
  & Sandhi.process
  & S.toList_

lexemesFrom :: LexerError m => Pos -> LBS.ByteString -> m [Sandhi.Token]
lexemesFrom pos0 bytes = bytes
  & tokensFrom pos0
  & tokensToLexemes

lexemes :: LexerError m => LBS.ByteString -> m [Sandhi.Token]
lexemes = lexemesFrom startPos


-- TODO move to Language.CCS

cstsFrom :: LexerError m => Pos -> LBS.ByteString -> m ([Parser.ParseError], Maybe [Parser.CST])
cstsFrom pos0 bytes = lexemesToCstsFrom pos0 <$> lexemesFrom pos0 bytes

csts :: LexerError m => LBS.ByteString -> m ([Parser.ParseError], Maybe [Parser.CST])
csts = cstsFrom startPos

lexemesToCstsFrom :: Pos -> [Sandhi.Token] -> ([Parser.ParseError], Maybe [Parser.CST])
lexemesToCstsFrom pos0 = Parser.parse pos0

lexemesToCsts :: [Sandhi.Token] -> ([Parser.ParseError], Maybe [Parser.CST])
lexemesToCsts = lexemesToCstsFrom startPos
