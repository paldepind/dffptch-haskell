{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DffptchSpec where

import Test.Hspec
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Text()
import GHC.Generics

import Dffptch.Internal

array :: [Value] -> Value
array = Array . V.fromList

-- Convenience functions
addDelta list = Delta (H.fromList list) H.empty [] H.empty
delDelta dels = Delta H.empty H.empty dels H.empty
modDelta list = Delta H.empty (H.fromList list) [] H.empty
recDelta recs = Delta H.empty H.empty [] (H.fromList recs)

dummyUser = object [ "name" .= String "Simon"
                   , "age" .= Number 21
                   , "male" .= True
                   ]

dummyUser2 = object [ "name" .= String "Simon"
                    , "age" .= Number 21
                    , "location" .= String "Denmark"
                    , "male" .= Bool True
                    ]

dummyUser3 = object [ "name" .= String "Simon"
                    , "age" .= Number 21
                    , "male" .= Bool True
                    , "occupation" .= String "Programmer"
                    ]

dummyUser4 = object [ "name" .= String "Simon"
                    , "age" .= Number 21
                    , "male" .= Bool True
                    , "sex" .= String "Male"
                    , "occupation" .= String "Programmer"
                    ]

dummyUser5 = object [ "name" .= String "Simon"
                    , "age" .= Number 21
                    ]

dummyUser6 = object [ "age" .= Number 21
                    ]

dummyUser7 = object [ "name" .= String "Simon"
                    , "age" .= Number 21
                    , "male" .= Bool False
                    ]

dummyHorse = object [ "type" .= String "horse"
                    , "parent" .= object
                        [ "type" .= String "goat"
                        , "color" .= String "brown"
                        ]
                    ]

dummyHorse2 = object [ "type" .= String "horse"
                     , "parent" .= object
                         [ "type" .= String "goat"
                         , "color" .= String "blue"
                         ]
                     ]

dummyGoat = object [ "type" .= String "goat"
                   , "parent" .= object
                       [ "type" .= String "goat"
                       , "color" .= String "grey"
                       , "parent" .= object
                           [ "type" .= String "horse"
                           , "color" .= String "light-grey"
                           ]
                       ]
                   ]

arr1 = Array $ V.fromList [Number 1, Number 2, Number 3]

arr2 = Array $ V.fromList [Number 1, Number 4, Number 3]

nestedArr1 = object [ "arr" .= arr1 ]
nestedArr2 = object [ "arr" .= arr2 ]

data Animal = Animal
  { color :: String
  , name :: String
  , age :: Int
  , alive :: Bool
  } deriving (Show, Generic)

instance ToJSON Animal
instance FromJSON Animal

animal1 = Animal
  { color = "brown"
  , name = "Thumper"
  , age = 12
  , alive = True
  }

animal2 = Animal
  { color = "lightbrown"
  , name = "Thumper"
  , age = 13
  , alive = False
  }

main :: IO ()
main = hspec $ do
  describe "toSortedList" $
    it "return a list from hashmap sorted by keys" $
      (toSortedList . H.fromList) ["b" .= Number 2, "foo" .= Number 0, "a" .= Number 1]
      `shouldBe` ["a" .= Number 1, "b" .= Number 2, "foo" .= Number 0]

  describe "diff" $ do
    it "return empty object on equal object" $
      diff dummyUser dummyUser `shouldBe` emptyDelta

    it "detects added field" $
      diff dummyUser dummyUser2 `shouldBe` addDelta [("location", String "Denmark")]

    it "detects added field at end" $
      diff dummyUser dummyUser3 `shouldBe` addDelta [("occupation", String "Programmer")]

    it "detects several added fields at end" $
      diff dummyUser dummyUser4 `shouldBe`
      addDelta [ ("occupation", (String "Programmer"))
               , ("sex", (String "Male"))
               ]
    it "detects deleted fields at end" $
      diff dummyUser dummyUser5 `shouldBe` delDelta [1]

    it "detects several deleted fields at end" $
      diff dummyUser dummyUser6 `shouldBe` delDelta [2, 1]

    it "detects modified fields" $
      diff dummyUser dummyUser7 `shouldBe` modDelta [("1", Bool False)]

    it "diffs recursively" $
      diff dummyHorse dummyHorse2 `shouldBe`
        recDelta [("0", modDelta [("0", String "blue")])]

    it "handles changes in arrays" $
      diff arr1 arr2 `shouldBe` modDelta [("1", Number 4)]

    it "handles changes in nested arrays" $
      diff nestedArr1 nestedArr2 `shouldBe`
        recDelta [("0", modDelta [("1", Number 4)])]

    it "is convertable to json" $
      toJSON (diff dummyUser dummyUser2) `shouldBe`
        object ["a" .= object ["location" .= String "Denmark"]]

  describe "parsing deltas" $ do
    it "parses empty delta" $
      decode "{}" `shouldBe` Just emptyDelta

    it "parses delta with only additions" $
      decode "{\"a\":{\"location\":\"Denmark\"}}" `shouldBe` 
        Just (addDelta [("location", String "Denmark")])

    it "parses delta with only additions" $
      decode "{\"m\":{\"1\":\"Denmark\"}}" `shouldBe` 
        Just (modDelta [("1", String "Denmark")])

    it "parses delta with deletion and recursion" $
      decode "{\"d\":[1,2],\"r\":{\"0\":{\"m\":{\"1\":\"Denmark\"}}}}" `shouldBe` 
        Just (Delta H.empty H.empty [1, 2] (H.fromList [("0", modDelta [("1", "Denmark")])]))
        -- Just (modDelta [("1", String "Denmark")])

  describe "patch" $ do
    it "handles modified field" $
      patch dummyUser (diff dummyUser dummyUser7) `shouldBe` dummyUser7

    it "handles deleted fields" $
      patch dummyUser (diff dummyUser dummyUser6) `shouldBe` dummyUser6

    it "handles added fields" $
      patch dummyUser (diff dummyUser dummyUser2) `shouldBe` dummyUser2

    it "handles recursively changed fields" $
      patch dummyHorse (diff dummyHorse dummyHorse2) `shouldBe` dummyHorse2

    it "applies changes to arrays" $
      patch arr1 (diff arr1 arr2) `shouldBe` arr2

  describe "diffing and patching recods" $ do
    it "diffs records" $
      diff animal1 animal2 `shouldBe` modDelta [("0", Number 13), ("1", Bool False), ("2", "lighbrown")]
