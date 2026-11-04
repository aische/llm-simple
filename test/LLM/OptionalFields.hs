-- | A structured-output type with every kind of optional field
-- autodocodec offers, for the strict-schema tests (P3 in fixes-043).
module LLM.OptionalFields
  ( Optionals (..),
    Inner (..),
    Node (..),
    innerDefaults,
  )
where

import Autodocodec qualified as AC
import Data.Aeson (FromJSON)
import Data.Text (Text)

data Inner = Inner
  { innerA :: Maybe Text,
    innerB :: Int
  }
  deriving (Eq, Show)

-- | The value of an 'Inner' whose fields were all left out.
innerDefaults :: Inner
innerDefaults = Inner Nothing 3

instance AC.HasCodec Inner where
  codec =
    AC.object "Inner" $
      Inner
        <$> AC.optionalField "a" "optional" AC..= (.innerA)
        <*> AC.optionalFieldWithDefault "b" 3 "with default" AC..= (.innerB)

data Optionals = Optionals
  { reqMaybe :: Maybe Text,
    orNull :: Maybe Text,
    opt :: Maybe Text,
    withDefault :: Int,
    omitted :: Int,
    nested :: Maybe Inner,
    named :: Maybe Inner,
    list :: [Inner],
    union :: Either Inner Int,
    plain :: Text
  }
  deriving (Eq, Show)
  deriving (FromJSON) via (AC.Autodocodec Optionals)

instance AC.HasCodec Optionals where
  codec =
    AC.object "Optionals" $
      Optionals
        <$> AC.requiredField "req_maybe" "required, Maybe" AC..= (.reqMaybe)
        <*> AC.optionalFieldOrNull "or_null" "optionalFieldOrNull" AC..= (.orNull)
        <*> AC.optionalField "opt" "optionalField" AC..= (.opt)
        <*> AC.optionalFieldWithDefault "def" 5 "optionalFieldWithDefault" AC..= (.withDefault)
        <*> AC.optionalFieldWithOmittedDefault "omit" 5 "optionalFieldWithOmittedDefault" AC..= (.omitted)
        <*> AC.optionalField "nested" "nested object" AC..= (.nested)
        <*> AC.optionalFieldWith "named" (AC.named "NamedInner" AC.codec) "named object ($ref)" AC..= (.named)
        <*> AC.requiredField "list" "objects in an array" AC..= (.list)
        <*> AC.requiredField "union" "Either (oneOf)" AC..= (.union)
        <*> AC.requiredField "plain" "required, not nullable" AC..= (.plain)

-- | A recursive type: its schema refers to itself through @$ref@.
data Node = Node
  { label :: Text,
    next :: Maybe Node
  }
  deriving (Eq, Show)
  deriving (FromJSON) via (AC.Autodocodec Node)

instance AC.HasCodec Node where
  codec =
    AC.named "Node" $
      AC.object "Node" $
        Node
          <$> AC.requiredField "label" "label" AC..= (.label)
          <*> AC.optionalField "next" "next node" AC..= (.next)
