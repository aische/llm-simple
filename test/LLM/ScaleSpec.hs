-- | Large inputs that used to take quadratic time (C12, C20). Each check,
-- assertions included (results may be lazy), runs under a timeout that the
-- linear code meets with a wide margin and the quadratic code does not.
module LLM.ScaleSpec (spec) where

import Control.Monad (forM_, replicateM)
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import LLM.Agent.ToolUtils (windowOffset)
import LLM.Agent.Tools.HistoryTool (chunkBackward, findNthUserBack)
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.Types
  ( ChatResponse (..),
    LLMTextResult,
    ToolCall (..),
    Turn (..),
    projectText,
    textPart,
    pattern UserTurn,
  )
import LLM.Core.Utils (getToolCalls)
import LLM.Providers.Claude (claudeProvider)
import LLM.Providers.Gemini (geminiProvider)
import LLM.Providers.OpenAI (openAIProvider)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

-- | Seconds a scaled-up check may take.
limitSeconds :: Int
limitSeconds = 10

within :: IO a -> IO a
within act = do
  r <- timeout (limitSeconds * 1_000_000) act
  case r of
    Just a -> pure a
    Nothing -> expectationFailure ("took longer than " <> show limitSeconds <> " s") >> fail "timeout"

-- | A reader that returns @bytes@ in pieces of @n@ bytes.
chunkedReader :: Int -> ByteString -> IO (IO ByteString)
chunkedReader n bytes = do
  ref <- newIORef bytes
  pure $ do
    rest <- readIORef ref
    let (chunk, rest') = BS.splitAt n rest
    writeIORef ref rest'
    pure chunk

parseWith :: LLMProvider -> ByteString -> IO LLMTextResult
parseWith = parseInChunksOf 4096

parseInChunksOf :: Int -> LLMProvider -> ByteString -> IO LLMTextResult
parseInChunksOf n p bytes = do
  reader <- chunkedReader n bytes
  p.parseStream (object []) reader (\_ -> pure ())

-- | How many fragments the stream tests send, and their size.
fragments :: Int
fragments = 40_000

fragment :: ByteString
fragment = BC.replicate 100 'a'

expectedArg :: T.Text
expectedArg = T.replicate (fragments * 100) "a"

claudeEvent :: ByteString -> ByteString -> ByteString
claudeEvent name dat = "event: " <> name <> "\ndata: " <> dat <> "\n\n"

claudeStart :: ByteString
claudeStart = claudeEvent "message_start" "{\"type\":\"message_start\",\"message\":{\"model\":\"claude-test\",\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}"

claudeEnd :: ByteString
claudeEnd =
  claudeEvent "message_delta" "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}"
    <> claudeEvent "message_stop" "{\"type\":\"message_stop\"}"

-- | The old paging code, kept as the reference for the rewrite.
oldChunkBackward :: Int -> [Turn] -> [[Turn]]
oldChunkBackward _ [] = []
oldChunkBackward n conv
  | n <= 0 = [conv]
  | otherwise = reverse (go (length conv) [])
  where
    go 0 acc = acc
    go end acc =
      let start = oldFindNthUserBack n (take end conv)
          page = take (end - start) (drop start conv)
       in if start >= end then page : acc else go start (page : acc)

oldFindNthUserBack :: Int -> [Turn] -> Int
oldFindNthUserBack n conv = go (length conv - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case conv !! idx of
          UserMessage _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

-- | Every conversation of up to @len@ turns over user, assistant and tool
-- turns, each turn numbered so pages can be told apart.
allConversations :: Int -> [[Turn]]
allConversations len = concatMap shapes [0 .. len]
  where
    shapes k = map (zipWith mk [0 :: Int ..]) (replicateM k "uat")
    mk i 'u' = UserTurn (T.pack (show i))
    mk i 'a' = AssistantMessage [textPart (T.pack (show i))]
    mk _ _ = ToolTurn []

-- | Alternating user and assistant turns.
longConversation :: Int -> [Turn]
longConversation pairs = concat [[UserTurn "q", AssistantMessage [textPart "a"]] | _ <- [1 .. pairs]]

spec :: Spec
spec = describe "Scale (linear time)" $ do
  describe "SSE" $
    it "reads a long line that arrives one byte at a time" $ do
      let size = 1024 * 1024
          body =
            "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"" <> BC.replicate size 'x' <> "\"},\"finish_reason\":null}]}\n\n"
              <> "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n"
      within $ do
        result <- parseInChunksOf 1 (openAIProvider "key") body
        fmap (T.length . (.respText)) result `shouldBe` Right size

  describe "stream assembly" $ do
    it "Claude: joins many tool-argument and thinking fragments" $ do
      let toolStart = claudeEvent "content_block_start" "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"f\",\"input\":{}}}"
          jsonDelta s = claudeEvent "content_block_delta" ("{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"" <> s <> "\"}}")
          stop i = claudeEvent "content_block_stop" ("{\"type\":\"content_block_stop\",\"index\":" <> i <> "}")
          thinkStart = claudeEvent "content_block_start" "{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}"
          thinkDelta = claudeEvent "content_block_delta" ("{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"" <> fragment <> "\"}}")
          body =
            claudeStart
              <> toolStart
              <> jsonDelta "{\\\"k\\\":\\\""
              <> BS.concat (replicate fragments (jsonDelta fragment))
              <> jsonDelta "\\\"}"
              <> stop "0"
              <> thinkStart
              <> BS.concat (replicate fragments thinkDelta)
              <> stop "1"
              <> claudeEnd
      within $ do
        result <- parseWith (claudeProvider "key") body
        case result of
          Right resp -> do
            map (.tcArguments) (getToolCalls resp) `shouldBe` [object ["k" .= expectedArg]]
            resp.respReasoning `shouldBe` Just expectedArg
          Left err -> expectationFailure (show err)

    it "OpenAI: joins many tool-argument and reasoning fragments" $ do
      let chunk delta = "data: {\"choices\":[{\"index\":0,\"delta\":" <> delta <> ",\"finish_reason\":null}]}\n\n"
          toolDelta s = chunk ("{\"tool_calls\":[{\"index\":0,\"id\":\"t1\",\"function\":{\"name\":\"f\",\"arguments\":\"" <> s <> "\"}}]}")
          reasoning = chunk ("{\"reasoning_content\":\"" <> fragment <> "\"}")
          body =
            BS.concat (replicate fragments reasoning)
              <> toolDelta "{\\\"k\\\":\\\""
              <> BS.concat (replicate fragments (toolDelta fragment))
              <> toolDelta "\\\"}"
              <> "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\ndata: [DONE]\n\n"
      within $ do
        result <- parseWith (openAIProvider "key") body
        case result of
          Right resp -> do
            map (.tcArguments) (getToolCalls resp) `shouldBe` [object ["k" .= expectedArg]]
            resp.respReasoning `shouldBe` Just expectedArg
          Left err -> expectationFailure (show err)

    it "Gemini: collects the parts of many chunks in order" $ do
      let chunk i = "data: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"" <> BC.pack (show i) <> ",\"}]}}]}\n\n"
          body = BS.concat (map chunk [1 .. fragments]) <> "data: {\"candidates\":[{\"finishReason\":\"STOP\"}]}\n\n"
      within $ do
        result <- parseWith (geminiProvider "key") body
        case result of
          Right resp -> resp.respText `shouldBe` T.concat [T.pack (show i) <> "," | i <- [1 .. fragments]]
          Left err -> expectationFailure (show err)

  describe "history paging and windowing" $ do
    it "chunkBackward and findNthUserBack match the old code on every short conversation" $
      forM_ (allConversations 8) $ \conv ->
        forM_ [0 .. 4] $ \n -> do
          chunkBackward n conv `shouldBe` oldChunkBackward n conv
          findNthUserBack n conv `shouldBe` oldFindNthUserBack n conv

    it "pages a long hidden history" $ do
      let conv = longConversation 50_000
      within $ do
        let pages = chunkBackward 1 conv
        length pages `shouldBe` 50_000
        map projectText [p | UserMessage p <- concat (take 1 pages)] `shouldBe` ["q"]

    it "finds a large window in a long conversation" $ do
      let conv = longConversation 50_000
      within $ windowOffset (Just 25_000) conv `shouldBe` 50_000
