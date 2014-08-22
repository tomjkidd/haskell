module SimpleJSON
    (
        JValue(..), -- Here the (..) indicates that we are exporting both the type and all of its constructors
        getString,
        getInt,
        getDouble,
        getBool,
        getObject,
        getArray,
        isNull
    ) where

data JValue = JString String
           | JNumber Double
           | JBool Bool
           | JNull
           | JObject [(String, JValue)]
           | JArray [JValue]
              deriving (Eq, Ord, Show)
{- Each value constructor in the JValue algebraic data type represent the 
valid types in JSON.
Note that JString, JNumber, and JBool take native typed data.
Note also that JValue is used recursively to define JObject and JArray.
-}

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull