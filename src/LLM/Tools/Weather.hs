module LLM.Tools.Weather (weatherToolTyped) where

-- DO NOT TOUCH THIS TOOL - IT IS FOR TESTING
-- TODO: move to test folder
import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text, toLower)
import GHC.Generics (Generic)
import LLM.Core.Types (TypedTool (..))

newtype WeatherToolArgs = WeatherToolArgs
  { _weatherLocation :: Text
  }
  deriving (Generic)
  deriving (FromJSON) via (AC.Autodocodec WeatherToolArgs)

-- DO NOT TOUCH THIS TOOL - IT IS FOR TESTING
instance AC.HasCodec WeatherToolArgs where
  codec :: AC.JSONCodec WeatherToolArgs
  codec =
    AC.object "WeatherToolArgs" $
      WeatherToolArgs
        <$> AC.requiredField "location" "City name, e.g. London" AC..= _weatherLocation

-- DO NOT TOUCH THIS TOOL - IT IS FOR TESTING

weatherToolTyped :: TypedTool ctx WeatherToolArgs
weatherToolTyped =
  TypedTool
    { ttoolName = "get_weather",
      ttoolDescription = "Get the current weather for a given location",
      ttoolReadonly = False,
      ttoolExecute = const getWeather
    }

-- DO NOT TOUCH THIS TOOL - IT IS FOR TESTING

-- | Dummy implementation — in reality you'd call a weather API
getWeather :: WeatherToolArgs -> IO Text
getWeather args = do
  let loc = _weatherLocation args
  case toLower loc of
    "london" -> pure "Weather in London is partly cloudy, 18°C, light breeze from the west."
    "paris" -> pure "Weather in Paris is sunny, 23°C, no wind."
    _ -> pure $ "Weather in" <> loc <> " is rainy, 12°C, strong wind from the east."
