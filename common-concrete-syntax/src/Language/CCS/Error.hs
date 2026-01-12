module Language.CCS.Error
  ( ReaderHooks(..)
  , ReaderError(..)
  , ReaderStyle(..)
  ) where

import Data.Text (Text)
import Language.Location (Span, Pos)
import Language.Text (SrcText)
import Language.CCS.Lexer.Cover (EolType)

import qualified Language.CCS.Types.Sandhi as Sandhi
import qualified Language.CCS.Types.Indentation as Indentation

data ReaderStyle
  = ExpectingWhitespaceAfterSeparator
    { styleLoc :: Span
    , theSeparator :: Sandhi.Token
    , afterSeparator :: Indentation.Token
    }
  | TrailingWhitespace SrcText
  | MissingNlAtEof { styleLoc :: Span }
  deriving (Show)

data ReaderError
  -- TODO someday, I'll distinguish between decoding errors and illegal codepoints
  -- also bad bytes are just being replaced by the unicode Replacement Character
  = IllegalBytesOrChars SrcText
  | IllegalBytesOrCharsInString SrcText

  -- whitespace stuff
  | LeadingWhitespace SrcText
  | MixedWhitespaceInIndentation SrcText
  | LeadingWhitespaceBeforeFirstIndent SrcText
  | ExcessIndentationBeforeMultilineClose SrcText
  | TooLittleIndentationInMultiline
    { theTooLittleIndentation :: SrcText -- ^ offending indentation
    -- , multilineStartsWith :: SrcText -- ^ open delimiter TODO
    , expectedIndentation :: Text
    }
  | MixedNewlines
    { errorLoc :: Span
    , foundNlType :: EolType
    , expectedNlType :: EolType
    }
  -- | indentated blocks must be directly enclosed, or preceded by colon, or preceded with a backslash (meaning line-continuation)
  | UnexpectedIndent
    { errorLoc :: Span
    , beforeIndent :: Maybe Indentation.Token
    }
  | CrammedTokens
    { errorLoc :: Span
    , beforeCrammed :: Indentation.Token
    , afterCrammed :: Indentation.Token
    }

  -- punctuation stuff
  | TooManyDots SrcText
  | TooManyColons SrcText
  | UnexpectedDot
    { errorLoc :: Span
    , beforeDot :: Maybe Indentation.Token
    , afterDot :: Maybe Indentation.Token
    }
  | UnexpectedColon
    { errorLoc :: Span
    , beforeColon :: Maybe Indentation.Token
    , afterColon :: Maybe Indentation.Token
    }
  | UnexpectedBackslash
    { errorLoc :: Span
    , afterBacklash :: Maybe Indentation.Token
    }

  -- atom stuff
  | ExpectingFractionalDigits Span
  | InvalidExponentOnInteger Span
  | ExpectingCloseQuote
    { expectingCloseQuoteAt :: Pos
    -- , quoteOpenedAt :: Span -- TODO
    }
  | ExpectingMultilineDelimiter
    { expectingCloseQuoteAt :: Pos
    -- , expectingDelimiter :: Text -- TODO
    }

  -- TODO from parsing
  | UnexpectedEndOfInput Span
  | Expecting String Sandhi.Token
  deriving (Show)

-- | The 'ReaderHooks' monad allows the caller to decide:
-- - how to report the diagnostic (or ignore)
-- - whether to recover from an error (where possible)
class (Monad m) => ReaderHooks m where
  -- | An ordinary part of the input was dropped (ie a comment token).
  -- For most purposes, we'd just strip comments out of the token stream with no fanfare.
  -- However, we'd like to support sophisticated callers which might like to:
  -- - search comments for tags like todo or debug
  -- - lint the comment text for style (like looking for typos, or ensuring there's a space after the hash)
  -- - keep the comment around for later, when perhaps it attaches to some nearby bit of code
  --   (this is often used for documentation, but I don't recommend it personally.
  --   I think comments should be there for the person who is reading the code, and no one and nothing else.)
  ignore :: SrcText -> m ()
  styleNote :: ReaderStyle -> m ()
  recoverableError :: ReaderError -> m ()
  fatalError :: ReaderError -> m a
