-- | JSON Schema helpers for structured output.
--
-- OpenAI's strict mode needs every property listed in @required@; an
-- optional property is emulated by making it nullable
-- ('LLM.Core.ProviderUtils.normalizeSchemaOpenAI'). The model then answers
-- @null@ for a field it leaves out, which the decoder of an
-- @optionalField*@ codec does not accept. 'dropOptionalNulls' removes those
-- @null@s again before decoding, using the schema the codec emitted.
module LLM.Core.Internal.Schema
  ( SchemaDefs,
    schemaDefs,
    isNullableSchema,
    optionalProperties,
    dropOptionalNulls,
    mapSubschemas,
    hoistDefs,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Named sub-schemas (@$defs@, @definitions@), and the root for @#@.
data SchemaDefs = SchemaDefs
  { sdRoot :: Value,
    sdNamed :: KM.KeyMap Value
  }

-- | The definitions anywhere in a schema. autodocodec puts the @$defs@ of a
-- named codec next to its first use, not at the root, so the whole schema
-- is searched. On a name clash the first one found wins.
schemaDefs :: Value -> SchemaDefs
schemaDefs root = SchemaDefs root (collectDefs "$defs" root `KM.union` collectDefs "definitions" root)

-- | The entries of one definitions keyword (@$defs@ or @definitions@)
-- anywhere in a schema, following sub-schema positions only; the node's
-- own entries first, then those below it in order.
collectDefs :: K.Key -> Value -> KM.KeyMap Value
collectDefs k (Object o) =
  foldr KM.union KM.empty $
    [d | Just (Object d) <- [KM.lookup k o]] <> map (collectDefs k) (subschemas o)
collectDefs _ _ = KM.empty

-- | Move every @$defs@ and @definitions@ entry to the root of the schema.
-- References like @#/$defs/<name>@ are pointers from the root, but
-- autodocodec puts the @$defs@ of a named codec next to its first use,
-- where OpenAI and Ollama do not find them. On a name clash the first one
-- found wins, as in 'schemaDefs'.
hoistDefs :: Value -> Value
hoistDefs root@(Object o) =
  Object (put "$defs" (put "definitions" (mapSubschemas strip o)))
  where
    put k obj
      | KM.null defs = KM.delete k obj
      | otherwise = KM.insert k (Object defs) obj
      where
        defs = fmap strip (collectDefs k root)
    -- A sub-schema without its definitions, at every depth.
    strip (Object x) = Object (mapSubschemas strip (KM.delete "$defs" (KM.delete "definitions" x)))
    strip v = v
hoistDefs v = v

-- | The schema a @$ref@ points to, for @#@, @#/$defs/<name>@ and
-- @#/definitions/<name>@. Other references are not resolved.
resolveRef :: SchemaDefs -> Text -> Maybe Value
resolveRef defs ref
  | ref == "#" = Just defs.sdRoot
  | Just name <- T.stripPrefix "#/$defs/" ref = named name
  | Just name <- T.stripPrefix "#/definitions/" ref = named name
  | otherwise = Nothing
  where
    named name = KM.lookup (K.fromText name) defs.sdNamed

-- | Whether a schema accepts @null@. A schema we cannot judge (an
-- unresolved @$ref@, a schema with no type constraint) counts as nullable,
-- so it is left alone.
isNullableSchema :: SchemaDefs -> Value -> Bool
isNullableSchema defs = go []
  where
    go seen (Object o)
      | Just (String ref) <- KM.lookup "$ref" o =
          ref `elem` seen || maybe True (go (ref : seen)) (resolveRef defs ref)
      | hasType "null" o = True
      | Just (Array vs) <- KM.lookup "enum" o = Null `elem` vs
      | Just c <- KM.lookup "const" o = c == Null
      | any (branches o) ["anyOf", "oneOf"] = True
      | Just (Array bs) <- KM.lookup "allOf" o = all (go seen) bs
      | any (`KM.member` o) constraining = False
      | otherwise = True
      where
        branches obj k = case KM.lookup k obj of
          Just (Array bs) -> any (go seen) bs
          _ -> False
    go _ (Bool b) = b
    go _ _ = True
    constraining = ["type", "anyOf", "oneOf", "allOf", "not", "properties", "items"]

-- | The properties of an object schema that are not in its @required@ list.
optionalProperties :: KM.KeyMap Value -> [K.Key]
optionalProperties o = case KM.lookup "properties" o of
  Just (Object props) -> filter (`notElem` requiredKeys o) (KM.keys props)
  _ -> []

requiredKeys :: KM.KeyMap Value -> [K.Key]
requiredKeys o = case KM.lookup "required" o of
  Just (Array ks) -> [K.fromText k | String k <- toList ks]
  _ -> []

hasType :: Text -> KM.KeyMap Value -> Bool
hasType t o = case KM.lookup "type" o of
  Just (String s) -> s == t
  Just (Array ts) -> String t `elem` ts
  _ -> False

-- | Remove the @null@ values of properties that the schema has optional and
-- not nullable, in nested objects and arrays too, so that the codec's
-- decoder sees them as absent. Everything else is kept as it is.
--
-- In a union (@anyOf@, @oneOf@) the walk continues only into the one branch
-- that fits the value (an object with the branch's required keys and no
-- unknown ones, or an array); with no or several such branches the value
-- is left alone.
dropOptionalNulls :: Value -> Value -> Value
dropOptionalNulls schema = walk [] schema
  where
    defs = schemaDefs schema

    -- @seen@: references followed since the last step into the value, so a
    -- reference cycle that does not consume the value stops.
    walk :: [Text] -> Value -> Value -> Value
    walk seen (Object s) v
      | Just (String ref) <- KM.lookup "$ref" s =
          case resolveRef defs ref of
            Just target | ref `notElem` seen -> walk (ref : seen) target v
            _ -> v
      | Just bs <- unionBranches s = case filter (fits seen v) bs of
          [b] -> walk seen b v
          _ -> v
    walk _ (Object s) (Object v) = Object (KM.mapMaybeWithKey (field s) v)
    walk _ (Object s) (Array vs)
      | Just items@(Object _) <- KM.lookup "items" s = Array (fmap (walk [] items) vs)
    walk _ _ v = v

    field s k x = case propertySchema s k of
      Just ps
        | x == Null,
          k `notElem` requiredKeys s,
          not (isNullableSchema defs ps) ->
            Nothing
        | otherwise -> Just (walk [] ps x)
      Nothing -> case KM.lookup "additionalProperties" s of
        Just aps@(Object _) -> Just (walk [] aps x)
        _ -> Just x

    propertySchema s k = case KM.lookup "properties" s of
      Just (Object props) -> KM.lookup k props
      _ -> Nothing

    unionBranches s =
      case mapMaybe (`KM.lookup` s) ["anyOf", "oneOf"] of
        [Array bs] -> Just (toList bs)
        _ -> Nothing

    -- Whether the value has the shape of this branch; only objects and
    -- arrays matter, since nothing is dropped from other values.
    fits seen v (Object b)
      | Just (String ref) <- KM.lookup "$ref" b =
          case resolveRef defs ref of
            Just target | ref `notElem` seen -> fits (ref : seen) v target
            _ -> False
    fits _ (Object v) (Object b) =
      (hasType "object" b || KM.member "properties" b)
        && all (`KM.member` v) (requiredKeys b)
        && ( case (KM.lookup "properties" b, KM.lookup "additionalProperties" b) of
               (_, Just (Object _)) -> True
               (_, Just (Bool True)) -> True
               (Just (Object props), _) -> all (`KM.member` props) (KM.keys v)
               _ -> KM.null v
           )
    fits _ (Array _) (Object b) = hasType "array" b || KM.member "items" b
    fits _ _ _ = False

-- | Apply a function to every direct sub-schema of a schema node: the
-- values of the keywords that hold a sub-schema, a list of them, or a map
-- of them. Everything else (property names, @enum@, @const@, @default@
-- and other literal values) is kept as it is.
mapSubschemas :: (Value -> Value) -> KM.KeyMap Value -> KM.KeyMap Value
mapSubschemas f = KM.mapWithKey sub
  where
    sub k v
      | k `elem` schemaMapKeywords = mapObject v
      | k `elem` schemaKeywords = schemaOrList v
      | otherwise = v
    mapObject (Object o) = Object (fmap f o)
    mapObject v = v
    schemaOrList (Array a) = Array (fmap f a)
    schemaOrList v = f v

-- | The direct sub-schemas of a schema node, in the positions
-- 'mapSubschemas' visits.
subschemas :: KM.KeyMap Value -> [Value]
subschemas o = concat [sub k v | (k, v) <- KM.toList o]
  where
    sub k v
      | k `elem` schemaMapKeywords, Object m <- v = KM.elems m
      | k `elem` schemaKeywords, Array a <- v = toList a
      | k `elem` schemaKeywords = [v]
      | otherwise = []

-- | Keywords whose value maps names to sub-schemas.
schemaMapKeywords :: [KM.Key]
schemaMapKeywords = ["properties", "patternProperties", "$defs", "definitions", "dependentSchemas"]

-- | Keywords whose value is a sub-schema or a list of sub-schemas.
schemaKeywords :: [KM.Key]
schemaKeywords =
  [ "items",
    "prefixItems",
    "additionalItems",
    "additionalProperties",
    "unevaluatedProperties",
    "unevaluatedItems",
    "propertyNames",
    "contains",
    "anyOf",
    "oneOf",
    "allOf",
    "not",
    "if",
    "then",
    "else"
  ]
