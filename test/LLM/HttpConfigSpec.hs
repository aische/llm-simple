module LLM.HttpConfigSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forever, void)
import Data.Aeson (Value, object)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import System.Timeout (timeout)
import LLM.Core.LLMProvider (genericGenerateText, genericStreamText)
import LLM.Core.ProviderUtils (mkLenientConfig)
import LLM.Core.Types (ChatRequest (..), LLMError (..), pattern UserTurn)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Providers.Ollama (ollamaProviderWith)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req (POST (..), ReqBodyJson (..), http, ignoreResponse, port, req, runReq, (/:))
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as NB
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

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

  describe "response body limits" $ do
    it "cuts an endless error body and returns HttpError" $
      withEndlessServer "500 Internal Server Error" "" $ \p -> do
        r <- within $ genericGenerateText (ollamaProviderWith (http "127.0.0.1") (port p)) (llmHooks noHooks) request
        case r of
          Left (HttpError 500 body) -> do
            T.length body `shouldSatisfy` (<= 64 * 1024 + 3)
            T.takeEnd 3 body `shouldBe` "..."
          other -> expectationFailure ("expected HttpError 500, got " <> take 200 (show other))

    it "cuts an endless error body of a stream and returns HttpError" $
      withEndlessServer "503 Service Unavailable" "" $ \p -> do
        r <- within $ genericStreamText (ollamaProviderWith (http "127.0.0.1") (port p)) (llmHooks noHooks) request (const (pure ()))
        case r of
          Left (HttpError 503 body) -> T.length body `shouldSatisfy` (<= 64 * 1024 + 3)
          other -> expectationFailure ("expected HttpError 503, got " <> take 200 (show other))

    it "turns an endless 200 body into a ParseError" $
      withEndlessServer "200 OK" "" $ \p -> do
        r <- within $ genericGenerateText (ollamaProviderWith (http "127.0.0.1") (port p)) (llmHooks noHooks) request
        case r of
          Left (ParseError msg) -> T.unpack msg `shouldBe` "response body exceeds 33554432 bytes"
          other -> expectationFailure ("expected ParseError, got " <> take 200 (show other))

    it "turns an endless stream event into a ParseError" $
      withEndlessServer "200 OK" "data: " $ \p -> do
        r <- within $ genericStreamText (ollamaProviderWith (http "127.0.0.1") (port p)) (llmHooks noHooks) request (const (pure ()))
        case r of
          Left (ParseError msg) -> T.unpack msg `shouldBe` "response body exceeds 8388608 bytes"
          other -> expectationFailure ("expected ParseError, got " <> take 200 (show other))

  describe "stream total size" $ do
    it "turns an endless stream of small events into a ParseError" $
      withRepeatingServer "200 OK" "" (BS.concat (replicate 1000 textEvent)) $ \p -> do
        r <- within $ genericStreamText (ollamaProviderWith (http "127.0.0.1") (port p)) (llmHooks noHooks) request (const (pure ()))
        case r of
          Left (ParseError msg) -> T.unpack msg `shouldBe` "response body exceeds 33554432 bytes"
          other -> expectationFailure ("expected ParseError, got " <> take 200 (show other))

    it "parses a stream just under the total cap" $ do
      -- 31 comment lines of 1 MiB (each under the per-line cap), then a
      -- short reply: about 31 MiB in all.
      let padding = BS.concat (replicate 31 (": " <> BS.replicate (1024 * 1024) 0x70 <> "\n"))
          body = padding <> textEvent <> finishEvent <> "data: [DONE]\n\n"
      BS.length body `shouldSatisfy` (< 32 * 1024 * 1024)
      withFixedServer "200 OK" body $ \p -> do
        r <- within $ genericStreamText (ollamaProviderWith (http "127.0.0.1") (port p)) (llmHooks noHooks) request (const (pure ()))
        case r of
          Right _ -> pure ()
          Left err -> expectationFailure ("expected a parsed stream, got " <> take 200 (show err))

-- | One OpenAI-style stream event with a text delta, and the final one.
textEvent, finishEvent :: BS.ByteString
textEvent = "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"x\"},\"finish_reason\":null}]}\n\n"
finishEvent = "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n"

-- | Fail instead of hanging when a body is read without a bound.
within :: IO a -> IO a
within act = timeout 20_000_000 act >>= maybe (fail "timed out: the body was read without a bound") pure

request :: ChatRequest
request =
  ChatRequest
    { reqModel = "test",
      reqConversation = [UserTurn "hi"],
      reqSystem = Nothing,
      reqTools = [],
      reqMaxTokens = 10,
      reqTemperature = Nothing,
      reqThinking = Nothing
    }

-- | A TCP server on a free local port that answers every POST with the
-- given status line, then the prefix, then @x@ bytes until the client
-- hangs up.
withEndlessServer :: BS.ByteString -> BS.ByteString -> (Int -> IO a) -> IO a
withEndlessServer status prefix = withRepeatingServer status prefix (BS.replicate 65536 0x78)

-- | Like 'withEndlessServer', repeating the given chunk.
withRepeatingServer :: BS.ByteString -> BS.ByteString -> BS.ByteString -> (Int -> IO a) -> IO a
withRepeatingServer status prefix chunk =
  withServer $ \conn -> do
    NB.sendAll conn ("HTTP/1.1 " <> status <> "\r\nContent-Length: 1000000000000\r\n\r\n" <> prefix)
    -- Stops when the client closes the connection and a send fails.
    void (try @SomeException (forever (NB.sendAll conn chunk)))

-- | A TCP server on a free local port that answers every POST with the
-- given status line and body.
withFixedServer :: BS.ByteString -> BS.ByteString -> (Int -> IO a) -> IO a
withFixedServer status body =
  withServer $ \conn ->
    void . try @SomeException $
      NB.sendAll conn ("HTTP/1.1 " <> status <> "\r\nContent-Length: " <> BC.pack (show (BS.length body)) <> "\r\n\r\n" <> body)

-- | A TCP server on a free local port that runs the handler for each POST.
withServer :: (N.Socket -> IO ()) -> (Int -> IO a) -> IO a
withServer handler action =
  bracket open N.close $ \sock -> do
    p <- fromIntegral <$> N.socketPort sock
    tid <- forkIO $ forever $ do
      (conn, _) <- N.accept sock
      void $ forkIO $ flip finally (N.close conn) $ do
        chunk <- NB.recv conn 4096
        if "POST" `BS.isPrefixOf` chunk then handler conn else pure ()
    action p `finally` killThread tid
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      pure sock

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
