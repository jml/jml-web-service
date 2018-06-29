module JmlSvc
  ( Config(..)
  , flags
  , run
  ) where

import Protolude

import Control.Monad.Logger
import qualified Network.Wai as Wai
import qualified Options.Applicative as Options

import qualified JmlSvc.Logging as Logging
import qualified JmlSvc.Metrics as Metrics
import qualified JmlSvc.RequestLog as RequestLog
import qualified JmlSvc.Warp as Warp

-- | Generic web server configuration.
data Config = Config
  { loggingConfig :: Logging.Config
  , metricsConfig :: Metrics.Config
  , requestLogConfig :: RequestLog.AccessLogLevel
  , warpConfig :: Warp.Config
  } deriving (Eq, Show)

-- | Command-line flags for generating 'Config'.
flags :: Options.Parser Config
flags = Config <$> Logging.flags <*> Metrics.flags <*> RequestLog.flags <*> Warp.flags

-- | Run a web server for 'app'. Blocks until the server is shut down.
run :: MonadIO io => Text -> Config -> Wai.Application -> io ()
run appName Config {..} app =
  Logging.run loggingConfig $ do
    prometheus <- Metrics.prometheus appName metricsConfig
    let logging = RequestLog.toMiddleware requestLogConfig
    Warp.run warpConfig (logging (prometheus app))
    logInfoN $ appName <> " terminated successfully"
