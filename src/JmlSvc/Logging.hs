-- | Configuration for "Control.Monad.Logger".
module JmlSvc.Logging
  ( flags
  , Config
  , run
  ) where

import Protolude

import qualified Control.Monad.Logger as Logger
import qualified Options.Applicative as Options

-- | Configuration for logging.
newtype Config = Config Logger.LogLevel deriving (Eq, Show)

-- | Command-line flags controlling logging.
flags :: Options.Parser Config
flags =
  Config <$> Options.option
  (Options.eitherReader (pure . fromKeyword . toS))
  (fold
    [ Options.long "log-level"
    , Options.help "Minimum severity for log messages"
    , Options.value Logger.LevelInfo
    ])

-- | Run a logging action using the given config.
run :: MonadIO io => Config -> Logger.LoggingT io a -> io a
run (Config severity) action =
  Logger.runStdoutLoggingT (Logger.filterLogger predicate action)
  where
    predicate _source level = level >= severity

type Keyword = Text

fromKeyword :: Keyword -> Logger.LogLevel
fromKeyword "error" = Logger.LevelError
fromKeyword "warn" = Logger.LevelWarn
fromKeyword "info" = Logger.LevelInfo
fromKeyword "debug" = Logger.LevelDebug
fromKeyword other = Logger.LevelOther other

_toKeyword :: Logger.LogLevel -> Keyword
_toKeyword Logger.LevelError = "error"
_toKeyword Logger.LevelWarn = "warn"
_toKeyword Logger.LevelInfo = "info"
_toKeyword Logger.LevelDebug = "debug"
_toKeyword (Logger.LevelOther other) = other
