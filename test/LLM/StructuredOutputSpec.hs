-- | How each provider's structured-output parser reports a response it
-- cannot use: empty content is 'EmptyResponse'; text that is not JSON, or a
-- body without the expected fields, is a 'ParseError' quoting the input.
module LLM.StructuredOutputSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.LLMProvider (LLMProvider (..))
import LLM.Core.Types (LLMError (..))
import LLM.Providers.Claude (claudeProvider)
import LLM.Providers.DeepSeek (deepSeekProvider)
import LLM.Providers.Gemini (geminiProvider)
import LLM.Providers.Ollama (ollamaProvider)
import LLM.Providers.OpenAI (openAIProvider)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)

-- | A provider, and how its API wraps the model's text in a response body.
data Case = Case
  { caseName :: String,
    caseProvider :: LLMProvider,
    caseBody :: Text -> Value,
    -- | Whether a body without the expected fields is a 'ParseError'
    -- (Gemini reports it as a missing candidate instead).
    caseShapeError :: Bool
  }

cases :: [Case]
cases =
  [ Case "OpenAI" (openAIProvider "") openAIBody True,
    Case "Ollama" ollamaProvider openAIBody True,
    Case "DeepSeek" (deepSeekProvider "") openAIBody True,
    Case "Claude" (claudeProvider "") claudeBody True,
    Case "Gemini" (geminiProvider "") geminiBody False
  ]
  where
    openAIBody t = object ["choices" .= [object ["message" .= object ["content" .= t]]]]
    claudeBody t = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= t]]]
    geminiBody t =
      object
        [ "candidates"
            .= [ object
                   [ "content" .= object ["parts" .= [object ["text" .= t]]],
                     "finishReason" .= ("STOP" :: Text)
                   ]
               ]
        ]

spec :: Spec
spec = describe "Structured-output parse errors" $
  mapM_ providerSpec cases

providerSpec :: Case -> Spec
providerSpec c = describe c.caseName $ do
  it "decodes valid JSON text" $ do
    r <- c.caseProvider.parseObjectResponse (c.caseBody "{\"answer\": 42}")
    fmap fst r `shouldBe` Right (object ["answer" .= (42 :: Int)])

  it "reports text that is not JSON as a ParseError quoting the text" $ do
    r <- c.caseProvider.parseObjectResponse (c.caseBody "Sure! Here is the object: {\"answer\":")
    expectParseError r ["not valid JSON", "Sure! Here is the object"]

  it "reports empty text as EmptyResponse" $ do
    r <- c.caseProvider.parseObjectResponse (c.caseBody "  ")
    fmap fst r `shouldBe` Left EmptyResponse

  if c.caseShapeError
    then it "reports a body without the expected fields as a ParseError quoting the body" $ do
      r <- c.caseProvider.parseObjectResponse (object ["unexpected" .= True])
      expectParseError r ["unexpected response", "\"unexpected\":true"]
    else pure ()

expectParseError :: Either LLMError a -> [Text] -> Expectation
expectParseError (Left (ParseError msg)) needles =
  case filter (not . (`T.isInfixOf` msg)) needles of
    [] -> pure ()
    missing -> expectationFailure $ "ParseError " <> show msg <> " lacks " <> show missing
expectParseError (Left err) _ = expectationFailure $ "expected ParseError, got " <> show err
expectParseError (Right _) _ = expectationFailure "expected ParseError, got Right"
