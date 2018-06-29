module JmlSvc.Warp
  ( Config(..)
  , flags
  , run
  ) where

import Protolude

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Options

-- | Generic Warp configuration.
data Config = Config
  { port :: Warp.Port -- ^ Port to listen on
  , debugExceptions :: Bool -- ^ Whether to show detailed exception information on 500s
  } deriving (Eq, Show)

-- | Command-line flags for generating 'Config'.
flags :: Options.Parser Config
flags =
  Config <$>
  Options.option Options.auto (fold [Options.long "port", Options.metavar "PORT", Options.help "Port to listen on"]) <*>
  Options.switch (fold [Options.long "debug-exceptions", Options.help "Show exceptions on 500."])

-- | Run a web server for 'app'. Blocks until the server is shut down.
run :: MonadIO io => Config -> Wai.Application -> io ()
run config@Config {..} app = liftIO $ Warp.runSettings (warpSettings config) app

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
