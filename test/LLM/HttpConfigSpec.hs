module LLM.HttpConfigSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forever, void)
import Data.Aeson (Value, object)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import LLM.Core.ProviderUtils (mkLenientConfig)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req (POST (..), ReqBodyJson (..), http, ignoreResponse, port, req, runReq, (/:))
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as NB
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "HTTP config" $ do
  it "sends a POST exactly once when the response times out" $ do
    posts <- newIORef (0 :: Int)
    -- Answer each request after 2 s; the client gives up after 100 ms.
    withSlowServer 2_000_000 (atomicModifyIORef' posts (\n -> (n + 1, ()))) $ \p -> do
      mgr <- HC.newManager HC.defaultManagerSettings {HC.managerResponseTimeout = HC.responseTimeoutMicro 100_000}
      r <-
        try $
          runReq (mkLenientConfig mgr) $
            req POST (http "127.0.0.1" /: "v1") (ReqBodyJson (object [] :: Value)) ignoreResponse (port p)
      case r of
        Left (_ :: SomeException) -> pure ()
        Right _ -> fail "expected the request to time out"
      readIORef posts >>= (`shouldBe` 1)

-- | A TCP server on a free local port that counts each request it receives
-- and answers it only after the given delay.
withSlowServer :: Int -> IO () -> (Int -> IO a) -> IO a
withSlowServer delay onRequest action =
  bracket open N.close $ \sock -> do
    p <- fromIntegral <$> N.socketPort sock
    tid <- forkIO $ forever $ do
      (conn, _) <- N.accept sock
      void $ forkIO $ flip finally (N.close conn) $ do
        chunk <- NB.recv conn 4096
        if "POST" `BS.isPrefixOf` chunk
          then do
            onRequest
            threadDelay delay
            NB.sendAll conn "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
          else pure ()
    action p `finally` killThread tid
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      pure sock
