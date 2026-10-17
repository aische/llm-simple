module LLM.LLMProviderSpec (spec) where

import Data.ByteString qualified as BS
import LLM.Core.LLMProvider (LLMProvider (..), genericGenerateText)
import LLM.Core.Types (ChatRequest (..), LLMError (..), pattern UserTurn)
import LLM.Generate.GenerateUtils (llmHooks)
import LLM.Generate.Logger (noHooks)
import LLM.Providers.OpenAI (openAIProvider)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec = describe "LLMProvider" $ do
  describe "non-streaming error responses" $ do
    it "keeps a JSON error body as the raw JSON text" $ do
      let raw = "{\"error\":{\"message\":\"bad key\"}}"
      result <- generateWith 401 raw
      result `shouldBe` Just (HttpError 401 "{\"error\":{\"message\":\"bad key\"}}")

    it "keeps the status of a non-JSON error body" $ do
      result <- generateWith 502 "<html>Bad Gateway</html>"
      result `shouldBe` Just (HttpError 502 "<html>Bad Gateway</html>")

    it "reports a non-JSON 200 body as a ParseError" $ do
      result <- generateWith 200 "<html>oops</html>"
      case result of
        Just (ParseError _) -> pure ()
        other -> expectationFailure ("expected ParseError, got " <> show other)

-- | Run a non-streaming chat against a provider that answers with the given
-- status and raw body; return the error, if any.
generateWith :: Int -> BS.ByteString -> IO (Maybe LLMError)
generateWith status raw = do
  let p = (openAIProvider "key") {sendRequest = \_ -> pure (status, raw)}
  result <- genericGenerateText p (llmHooks noHooks) request
  pure (either Just (const Nothing) result)

request :: ChatRequest
request =
  ChatRequest
    { reqModel = "gpt-test",
      reqConversation = [UserTurn "hi"],
      reqSystem = Nothing,
      reqTools = [],
      reqMaxTokens = 10,
      reqTemperature = Nothing,
      reqThinking = Nothing
    }
