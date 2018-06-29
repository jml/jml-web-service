module JmlSvc.Web
  ( Config(..)
  , AccessLogLevel
  , flags
  , run
  ) where

import Protolude

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as RL
import qualified Options.Applicative as Options

-- | Generic Warp configuration.
data Config = Config
  { port :: Warp.Port -- ^ Port to listen on
  , accessLogs :: AccessLogLevel -- ^ Level of access logs to display
  , debugExceptions :: Bool -- ^ Whether to show detailed exception information on 500s
  } deriving (Eq, Show)

-- | Command-line flags for generating 'Config'.
flags :: Options.Parser Config
flags =
  Config <$>
  Options.option Options.auto (fold [Options.long "port", Options.metavar "PORT", Options.help "Port to listen on"]) <*>
  Options.option
    (Options.eitherReader parseAccessLogs)
    (fold [Options.long "access-logs", Options.help "How to log HTTP access", Options.value Disabled]) <*>
  Options.switch (fold [Options.long "debug-exceptions", Options.help "Show exceptions on 500."])
  where

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


-- | Run a web server for 'app'. Blocks until the server is shut down.
run :: MonadIO io => Config -> Wai.Application -> io ()
run config@Config {..} app = liftIO $ Warp.runSettings settings (logging app)
  where
    settings = warpSettings config
    logging =
      case accessLogs of
        Disabled -> identity
        Enabled -> RL.logStdout
        DevMode -> RL.logStdoutDev

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Warp.Settings
warpSettings Config {..} =
  Warp.setOnExceptionResponse exceptionHandler . Warp.setBeforeMainLoop printPort . Warp.setPort port $
  Warp.defaultSettings
  where
    printPort = putText $ "Listening on: " <> show port
    exceptionHandler
      | debugExceptions = Warp.exceptionResponseForDebug
      | otherwise = Warp.defaultOnExceptionResponse
