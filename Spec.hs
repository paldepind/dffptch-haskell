{-# LANGUAGE OverloadedStrings #-}

module DffptchSpec where

import Test.Hspec
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Text()

import Dffptch.Internal

array :: [Value] -> Value
array = Array . V.fromList

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

main :: IO ()
main = hspec $ do
  describe "toSortedList" $
    it "return a list from hashmap sorted by keys" $
      (toSortedList . H.fromList) ["b" .= Number 2, "foo" .= Number 0, "a" .= Number 1]
      `shouldBe` ["a" .= Number 1, "b" .= Number 2, "foo" .= Number 0]

  describe "diff" $ do
    it "return empty object on equal object" $
      diff dummyUser dummyUser `shouldBe` Object H.empty

    it "detects added field" $
      diff dummyUser dummyUser2 `shouldBe`
      object ["a" .= object ["location" .= String "Denmark"]]

    it "detects added field at end" $
      diff dummyUser dummyUser3 `shouldBe`
      object ["a" .= object ["occupation" .= String "Programmer"]]

    it "detects several added fields at end" $
      diff dummyUser dummyUser4 `shouldBe`
      object ["a" .= object ["occupation" .= String "Programmer",
                                           "sex" .= String "Male"]]
    it "detects deleted fields at end" $
      diff dummyUser dummyUser5 `shouldBe`
      object ["d" .= (Array $ V.fromList [Number 1])]

    it "detects several deleted fields at end" $
      diff dummyUser dummyUser6 `shouldBe`
      object ["d" .= (Array $ V.fromList [Number 2, Number 1])]

    it "detects modified fields" $
      diff dummyUser dummyUser7 `shouldBe`
      object ["m" .= object ["1" .= Bool False]]

    it "diffs recursively" $
      diff dummyHorse dummyHorse2 `shouldBe`
      object ["r" .= object ["0" .=
        object ["m" .= object ["0" .= String "blue"]]
      ]]

    it "handles changes in arrays" $
      diff arr1 arr2
      `shouldBe` object ["m" .= object ["1" .= Number 4]]

    it "handles changes in nested arrays" $
      diff nestedArr1 nestedArr2 `shouldBe`
      object ["r" .= object ["0" .=
        object ["m" .= object ["1" .= Number 4]]
      ]]

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
