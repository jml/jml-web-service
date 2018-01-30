module JmlSvc.Metrics (Config(..), flags, prometheus) where

import Protolude

import qualified Control.Monad.Logger as Logger
import qualified Data.Default as Default
import GHC.Stats (getRTSStatsEnabled)
import qualified Network.Wai as Wai
import qualified Options.Applicative as Options

import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom

import qualified JmlSvc.Instrument as Instrument

-- | Generic web server configuration.
newtype Config = Config
  { enableGhcMetrics :: Bool -- ^ Whether to include Prometheus metrics for GHC runtime stats
  } deriving (Eq, Show)

-- | Command-line flags for generating 'Config'.
flags :: Options.Parser Config
flags =
  Config <$> Options.switch (fold [Options.long "ghc-metrics", Options.help "Export GHC metrics. Requires running with +RTS."])

-- | Run a web server for 'app'. Blocks until the server is shut down.
prometheus :: (Logger.MonadLogger m, MonadIO m) => Text -> Config -> m Wai.Middleware
prometheus appName Config {..} = do
  requests <- liftIO $ Prom.registerIO Instrument.requestLatency
  when enableGhcMetrics $ do
    statsEnabled <- liftIO getRTSStatsEnabled
    unless statsEnabled $ Logger.logWarnN "Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T."
    void . liftIO $ Prom.register Prom.ghcMetrics
  pure $ Instrument.prometheus Default.def requests appName
