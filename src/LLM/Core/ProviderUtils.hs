module LLM.Core.ProviderUtils
  ( lenientConfig,
    mkLenientConfig,
    readAll,
    readResponseBody,
    handleStreamResponse,
    normalizeSchemaOpenAI,
    stripJsonFences,
    decodeObjectText,
    unexpectedResponse,
    stripBoundsAndComments,
    decodeToolArguments,
    invalidToolArgumentsError,
  )
where

import Control.Exception (throwIO)
import Control.Retry (limitRetries)
import Data.Aeson (ToJSON (toJSON), Value (..), decodeStrict', eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import LLM.Core.Internal.Body
  ( ResponseTooLarge (..),
    maxResponseBodyBytes,
    readBodyPrefix,
    readErrorBody,
  )
import LLM.Core.Internal.Schema (isNullableSchema, mapSubschemas, optionalProperties, schemaDefs)
import LLM.Core.Types (LLMError (EmptyResponse, HttpError, ParseError), LLMResult, LLMTextResult)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Req (HttpConfig, defaultHttpConfig, httpConfigAltManager, httpConfigCheckResponse, httpConfigRetryPolicy)
import Network.HTTP.Types.Status (statusCode)
import System.IO.Unsafe (unsafePerformIO)

-- | The HTTP config every provider uses: 'mkLenientConfig' with one shared
-- TLS manager that has no response timeout.
lenientConfig :: HttpConfig
lenientConfig = mkLenientConfig sharedManager

-- | HTTP config for provider requests using the given manager:
--
-- * req does not throw on non-2xx; we handle errors ourselves.
-- * req never retries. A retry resends a paid POST, and retrying is the job
--   of 'LLM.Core.Utils.withRetry'.
mkLenientConfig :: HC.Manager -> HttpConfig
mkLenientConfig mgr =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing,
      httpConfigRetryPolicy = limitRetries 0,
      httpConfigAltManager = Just mgr
    }

-- | Manager shared by all provider requests. A non-streaming call receives
-- its response headers only after generation finishes, so http-client's
-- default 30 s response timeout would cut off long calls. The only response
-- deadline is the one the application configures ('LLM.Generate.ModelConfig.mcRequestTimeout').
sharedManager :: HC.Manager
sharedManager =
  unsafePerformIO $
    HC.newManager tlsManagerSettings {HC.managerResponseTimeout = HC.responseTimeoutNone}
{-# NOINLINE sharedManager #-}

-- | Read an error body for its message: at most 64 KiB, with @...@ as a
-- last chunk when the body went on. Reading stops there, so an endless
-- error body cannot exhaust memory.
readAll :: HC.BodyReader -> IO [BS.ByteString]
readAll br = pure <$> readErrorBody (HC.brRead br)

-- | Read a non-streaming response as @(status, body)@, for
-- 'LLM.Core.LLMProvider.sendRequest'. Use it with req's @reqBr@ instead of
-- @bsResponse@:
--
-- > reqBr POST url (ReqBodyJson body) opts readResponseBody
--
-- A non-200 body is cut to 64 KiB (see 'readAll'). A 200 body over 32 MiB
-- throws an exception that the @generic*@ functions in
-- "LLM.Core.LLMProvider" turn into a 'ParseError'.
readResponseBody :: HC.Response HC.BodyReader -> IO (Int, BS.ByteString)
readResponseBody resp = do
  let status = statusCode (HC.responseStatus resp)
      br = HC.responseBody resp
  if status /= 200
    then (status,) <$> readErrorBody (HC.brRead br)
    else do
      (bs, cut) <- readBodyPrefix maxResponseBodyBytes (HC.brRead br)
      if cut
        then throwIO (ResponseTooLarge maxResponseBodyBytes)
        else pure (status, bs)

-- | Decode tool-call arguments sent as a JSON string. Empty input means the
-- tool takes no arguments. 'Nothing' if the text is not valid JSON (for
-- example because the response was cut off by the token limit).
decodeToolArguments :: Text -> Maybe Value
decodeToolArguments t
  | T.null (T.strip t) = Just (object [])
  | otherwise = decodeStrict' (encodeUtf8 t)

-- | Error for a tool call whose arguments are unusable. Pass the provider's
-- stop reason when it says the response hit the token limit
-- (@max_tokens@ / @length@), so the message names the cause.
invalidToolArgumentsError :: Text -> Maybe Text -> LLMError
invalidToolArgumentsError name mTruncated =
  ParseError $
    "tool call "
      <> name
      <> case mTruncated of
        Just reason -> " was cut off (stop reason: " <> reason <> "); increase max tokens"
        Nothing -> ": arguments are not valid JSON"

-- | Handle streaming response: check status, read error body or delegate
-- to the provider-specific stream parser.
handleStreamResponse :: HC.Response HC.BodyReader -> (HC.BodyReader -> IO LLMTextResult) -> IO LLMTextResult
handleStreamResponse resp handler = do
  let status = statusCode (HC.responseStatus resp)
  if status /= 200
    then do
      chunks <- readAll (HC.responseBody resp)
      -- Lenient: an error page need not be UTF-8, and a decode exception
      -- here would escape the caller's 'HttpException' handler.
      pure $ Left $ HttpError status (decodeUtf8With lenientDecode (BS.concat chunks))
    else handler (HC.responseBody resp)

-- | Recursively enforce OpenAI structured output constraints: every object
-- node gets @additionalProperties: false@ and all its properties in
-- @required@. Strict mode allows no optional properties, so a property
-- that was optional and does not accept @null@ becomes nullable
-- (@anyOf: [<schema>, {"type": "null"}]@); the model answers @null@ to
-- leave it out, and 'LLM.Core.Internal.Schema.dropOptionalNulls' removes
-- that @null@ before decoding.
--
-- Strict mode refuses @oneOf@, so it becomes @anyOf@ (unless the node has
-- an @anyOf@ already). autodocodec emits @oneOf@ for disjoint unions such
-- as 'Either', whose branches cannot both match, so the meaning is the
-- same.
--
-- Only schema keywords are changed: the walk follows sub-schema positions,
-- so a property called @oneOf@ or a literal value is kept.
normalizeSchemaOpenAI :: Value -> Value
normalizeSchemaOpenAI schema = go schema
  where
    defs = schemaDefs schema
    go (Object o) =
      let o' = oneOfToAnyOf (mapSubschemas go o)
          fixed = case KM.lookup "type" o' of
            Just (String "object") ->
              let propKeys = case KM.lookup "properties" o' of
                    Just (Object p) -> KM.keys p
                    _ -> []
                  optional = optionalProperties o
                  withNull k p
                    | k `elem` optional && not (isNullableSchema defs p) =
                        object ["anyOf" .= [p, object ["type" .= ("null" :: Text)]]]
                    | otherwise = p
                  withNullable = case KM.lookup "properties" o' of
                    Just (Object p) -> KM.insert "properties" (Object (KM.mapWithKey withNull p)) o'
                    _ -> o'
                  withAP = KM.insert "additionalProperties" (Bool False) withNullable
                  withReq = KM.insert "required" (toJSON propKeys) withAP
               in withReq
            _ -> o'
       in Object fixed
    go v = v
    oneOfToAnyOf o = case KM.lookup "oneOf" o of
      Just bs | not (KM.member "anyOf" o) -> KM.insert "anyOf" bs (KM.delete "oneOf" o)
      _ -> o

stripJsonFences :: Text -> Text
stripJsonFences t =
  let t' = T.strip t
      t'' =
        if "```" `T.isPrefixOf` t'
          then T.strip . T.drop 1 . T.dropWhile (/= '\n') $ t'
          else t'
      t''' =
        if "```" `T.isSuffixOf` t''
          then T.strip $ T.dropEnd 3 t''
          else t''
   in t'''

-- | Decode the text a model returned for a structured-output request, after
-- 'stripJsonFences'. Text that is empty then is 'EmptyResponse'. Text that
-- is not valid JSON is a 'ParseError' with the decoder's message and the
-- start of the text.
decodeObjectText :: Text -> LLMResult Value
decodeObjectText raw
  | T.null text = Left EmptyResponse
  | otherwise = case eitherDecodeStrict' (encodeUtf8 text) of
      Left err ->
        Left $
          ParseError $
            "structured output is not valid JSON: " <> T.pack err <> "; text: " <> excerpt text
      Right v -> Right v
  where
    text = stripJsonFences raw

-- | 'ParseError' for a response body that does not have the expected
-- shape, with the parser's message and the start of the body.
unexpectedResponse :: String -> Value -> LLMError
unexpectedResponse err body =
  ParseError $
    "unexpected response: " <> T.pack err <> "; body: " <> excerpt (TL.toStrict (TLE.decodeUtf8 (encode body)))

-- | At most 200 characters of @t@, with @...@ when cut.
excerpt :: Text -> Text
excerpt t
  | T.length t > 200 = T.take 200 t <> "..."
  | otherwise = t

-- | Remove @minimum@, @maximum@ and @$comment@ keywords from a JSON Schema.
--
-- Only schema keywords are removed: the walk follows sub-schema positions
-- (@properties@, @items@, @anyOf@, @$defs@, ...). Property names and
-- literal values (@enum@, @const@, @default@, @examples@) are kept, so a
-- property called @minimum@ survives.
stripBoundsAndComments :: Value -> Value
stripBoundsAndComments (Object obj) =
  Object $
    mapSubschemas stripBoundsAndComments $
      KM.filterWithKey (\k _ -> k `notElem` ["minimum", "maximum", "$comment"]) obj
stripBoundsAndComments other = other
