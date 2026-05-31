module LLM.Load.Utils where

import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT (..), MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLM.Load.Types (LoadConfigError (..))

decodeJsonFile ::
  (FromJSON a, MonadIO m) =>
  FilePath ->
  (String -> err) ->
  ExceptT err m a
decodeJsonFile filePath tag =
  ExceptT $
    either (Left . tag) Right
      <$> liftIO (eitherDecodeFileStrict filePath)

getSystemPrompt :: Maybe Text -> ExceptT LoadConfigError IO (Maybe Text)
getSystemPrompt mbSystemPrompt =
  case mbSystemPrompt of
    Nothing -> pure Nothing
    Just t -> do
      if T.isPrefixOf "file:" t
        then do
          let filePath = drop 5 (T.unpack t)
          result <- liftIO (try (TIO.readFile filePath) :: IO (Either IOException Text))
          case result of
            Left (e :: IOException) -> throwError (LoadSystemPromptError (T.pack (show e)))
            Right content -> pure (Just content)
        else pure $ Just t
