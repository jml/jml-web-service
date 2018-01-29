-- | This module provides "Network.Wai" middlware for exporting "Prometheus"
-- metrics and for instrumenting WAI applications.
module JmlSvc.Instrument
  ( prometheus
  , requestLatency
  ) where

import Protolude

import qualified Data.ByteString.Builder as BS
import qualified Data.Default as Default
import Data.Ratio ((%))
import Data.String (String)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom
import System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)

-- | Settings that control the behavior of the Prometheus middleware.
data PrometheusSettings = PrometheusSettings
  { prometheusEndPoint :: [Text]
        -- ^ The path that will be used for exporting metrics. The default value
        -- is ["metrics"] which corresponds to the path /metrics.
  , prometheusHandlerName :: Maybe Text
        -- ^ The name of the handler used to record metrics about the Prometheus
        -- endpoint. If Nothing, then we won't record any.
  }

instance Default.Default PrometheusSettings where
  def = PrometheusSettings {prometheusEndPoint = ["metrics"], prometheusHandlerName = Just "metrics"}

type RequestLatency = Prom.Metric (Prom.Vector Prom.Label3 Prom.Histogram)

requestLatency :: IO RequestLatency
requestLatency = Prom.vector ("handler", "method", "status_code") $ Prom.histogram info Prom.defaultBuckets
  where
    info = Prom.Info "http_request_duration_seconds" "The HTTP request latencies in seconds."

-- | Instrument a WAI app with the default WAI metrics.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentApp ::
     RequestLatency -- ^ Metric to record thingy on
  -> Text -- ^ The label used to identify this app
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentApp metric handler app req respond = do
  start <- getTime Monotonic
  app req $ \res -> do
    end <- getTime Monotonic
    let method = toS (Wai.requestMethod req)
    let status = show @Int @String (HTTP.statusCode (Wai.responseStatus res))
    let latency = fromRational $ toRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)
    Prom.withLabel (toS handler, method, status) (Prom.observe latency) metric
    respond res

-- | Instrument an app with Prometheus and export metrics from the configured
-- handler.
prometheus ::
     PrometheusSettings -- ^ How we're going to use Prometheus
  -> RequestLatency -- ^ A metric to instrument with request information
  -> Text -- ^ The label used to identify the app
  -> Wai.Middleware
prometheus PrometheusSettings {..} duration appName app req respond =
  if Wai.requestMethod req == HTTP.methodGet && Wai.pathInfo req == prometheusEndPoint
    then case prometheusHandlerName of
           Nothing -> respondWithMetrics respond
           Just name -> instrumentApp duration name (const respondWithMetrics) req respond
    else instrumentApp duration appName app req respond

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
respondWithMetrics respond = do
  metrics <- Prom.exportMetricsAsText
  respond $ Wai.responseBuilder HTTP.status200 headers $ BS.byteString metrics
  where
    headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
