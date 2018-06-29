module JmlSvc.RequestLog
  ( AccessLogLevel
  , flags
  , toMiddleware
  ) where

import Protolude

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Options.Applicative as Options

-- | Command-line flags for generating 'Config'.
flags :: Options.Parser AccessLogLevel
flags =
  Options.option
    (Options.eitherReader parseAccessLogs)
    (fold [Options.long "access-log-level", Options.help "How to log HTTP access", Options.value Disabled])

-- | What level of access logs to show.
data AccessLogLevel
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Construct an access log level from a command-line-friendly string.
--
-- Throws an error in the given monad if it can't recognize the level name.
parseAccessLogs
  :: (IsString error, IsString levelName, MonadError error m, Eq levelName)
  => levelName -- ^ String-like thing describing the level. Either "none", "basic", or "dev".
  -> m AccessLogLevel  -- ^ The corresponding access level.
parseAccessLogs "none" = pure Disabled
parseAccessLogs "basic" = pure Enabled
parseAccessLogs "dev" = pure DevMode
parseAccessLogs _ = throwError "One of 'none', 'basic', or 'dev'"

-- | Construct a WAI middleware to log requests mased on the given access log level.
toMiddleware :: AccessLogLevel -> Wai.Middleware
toMiddleware Disabled = identity
toMiddleware Enabled = RL.logStdout
toMiddleware DevMode = RL.logStdoutDev
