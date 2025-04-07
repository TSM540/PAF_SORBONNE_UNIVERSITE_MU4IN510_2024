{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Data.Sequence as Seq (Seq, fromList) 
import Data.Map.Strict as Map (Map, fromList, foldrWithKey, empty, insert)
import Data.Text as Text (Text,pack)
data JSon = 
    JString Text
    | JNumber Double
    | JBool Bool
    | JNull
    | JArray (Seq JSon)
    | JObject (Map Text JSon)
    deriving (Show, Eq, Ord)

mkArray :: [JSon] -> JSon
mkArray = JArray . Seq.fromList

mkObject :: [(Text, JSon)] -> JSon
mkObject = JObject . Map.fromList

personSimon :: JSon
personSimon = JObject $ Map.fromList [
        ("name", JString "Bumstead"),
        ("prenom", JString "Chris"),
        ("age", JNumber 29),
        ("travail", mkObject[
            ("role",JString "Bodybuilder"),
            ("societe", JString "Olympia")
         ]
        ),
        ("contact",mkArray[
            mkObject[("mel",JString "12@gmail.com")],
            mkObject [("tel",JString "1234567890")]
        ])
    ]


-- >>> personSimon
-- JObject (fromList [("age",JNumber 29.0),("contact",JArray (fromList [JObject (fromList [("mel",JString "

infixr 5 ==>
(==>) :: Bool -> Bool -> Bool
(==>) x y = not x || y
class Encode a where
    toJson :: a -> JSon
-- Injectivite
lawEncodeInj :: (Encode a, Eq b) => a -> b -> Bool
lawEncodeInj x y = (x /= y)  ==> (toJson x /= toJson y)

instance Encode Double where
    toJson :: Double -> JSon
    -- toJson x = JNumber x
    toJson = JNumber

instance Encode Int where
    toJson :: Int -> JSon
    toJson = JNumber . fromIntegral

-- >>> toJson (42 :: Int)
-- JNumber 42.0

instance Encode () where 
    toJson :: () -> JSon
    toJson _ = JNull

instance Encode Bool where
    toJson :: Bool -> JSon
    toJson = JBool

instance Encode Text where
    toJson :: Text -> JSon
    toJson = JString

instance Encode (Seq a)  where 
    toJson :: Seq a -> JSon
    toJson = JArray . fmap toJson

class ShowText a where
    showText :: a -> Text
instance ShowText Text where
    showText :: Text -> Text
    showText = id


instance (ShowText k, Encode a) => Encode (Map k a) where
    toJson :: Map k a -> JSon
    toJson = JObject . (Map.foldrWithKey aux Map.empty)
        where
            aux clef val  = Map.insert (showText clef) (toJson val)
               