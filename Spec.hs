module DffptchSpec where

import Test.Hspec
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
--import qualified Data.Text.Lazy as L
import Data.Text (pack)


import Dffptch.Internal

objFromList = Object . H.fromList

dummyUser = objFromList [ (pack "name", String $ pack "Simon")
                        , (pack "age", Number 21)
                        , (pack "male", Bool True)
                        ]

dummyUser2 = objFromList [ (pack "name", String $ pack "Simon")
                         , (pack "age", Number 21)
                         , (pack "location", String $ pack "Denmark")
                         , (pack "male", Bool True)
                         ]

dummyUser3 = objFromList [ (pack "name", String $ pack "Simon")
                         , (pack "age", Number 21)
                         , (pack "male", Bool True)
                         , (pack "occupation", String $ pack "Programmer")
                         ]

dummyUser4 = objFromList [ (pack "name", String $ pack "Simon")
                         , (pack "age", Number 21)
                         , (pack "male", Bool True)
                         , (pack "sex", String $ pack "Male")
                         , (pack "occupation", String $ pack "Programmer")
                         ]

dummyUser5 = objFromList [ (pack "name", String $ pack "Simon")
                         , (pack "age", Number 21)
                         ]

dummyUser6 = objFromList [ (pack "age", Number 21)
                         ]

dummyUser7 = objFromList [ (pack "name", String $ pack "Simon")
                         , (pack "age", Number 21)
                         , (pack "male", Bool False)
                         ]

dummyHorse = objFromList [ (pack "type", String $ pack "horse")
                         , (pack "parent", objFromList
                             [ (pack "type", String $ pack "goat")
                             , (pack "color", String $ pack "brown")
                             ])
                         ]

dummyHorse2 = objFromList [ (pack "type", String $ pack "horse")
                          , (pack "parent", objFromList
                              [ (pack "type", String $ pack "goat")
                              , (pack "color", String $ pack "blue")
                              ])
                          ]

dummyGoat = objFromList [ (pack "type", String $ pack "goat")
                        , (pack "parent", objFromList
                            [ (pack "type", String $ pack "goat")
                            , (pack "color", String $ pack "grey")
                            , (pack "parent", objFromList
                                [ (pack "type", String $ pack "horse")
                                , (pack "color", String $ pack "light-grey")
                                ])
                            ])
                        ]

arr1 = Array $ V.fromList [Number 1, Number 2, Number 3]

arr2 = Array $ V.fromList [Number 1, Number 4, Number 3]

nestedArr1 = objFromList [ (pack "arr", arr1) ]
nestedArr2 = objFromList [ (pack "arr", arr2) ]

main :: IO ()
main = hspec $ do
  describe "toSortedList" $
    it "return a list from hashmap sorted by keys" $
      (toSortedList . H.fromList) [(pack "b", Number 2), (pack "foo", Number 0), (pack "a", Number 1)]
      `shouldBe` [(pack "a", Number 1), (pack "b", Number 2), (pack "foo", Number 0)]

  describe "diff" $ do
    it "returns its second argument" $
      diff (Number 1) (Number 2) `shouldBe` Number 2

    it "return empty object on equal object" $
      diff dummyUser dummyUser `shouldBe` Object H.empty

    it "detects added field" $
      diff dummyUser dummyUser2 `shouldBe`
      objFromList [(pack "a", objFromList [(pack "location", String $ pack "Denmark")])]

    it "detects added field at end" $
      diff dummyUser dummyUser3 `shouldBe`
      objFromList [(pack "a", objFromList [(pack "occupation", String $ pack "Programmer")])]

    it "detects several added fields at end" $
      diff dummyUser dummyUser4 `shouldBe`
      objFromList [(pack "a", objFromList [(pack "occupation", String $ pack "Programmer"),
                                           (pack "sex", String $ pack "Male")])]
    it "detects deleted fields at end" $
      diff dummyUser dummyUser5 `shouldBe`
      objFromList [(pack "d",  Array $ V.fromList [Number 1])]

    it "detects several deleted fields at end" $
      diff dummyUser dummyUser6 `shouldBe`
      objFromList [(pack "d", Array $ V.fromList [Number 2, Number 1])]

    it "detects modified fields" $
      diff dummyUser dummyUser7 `shouldBe`
      objFromList [(pack "m", objFromList [(pack "1", Bool False)])]

    it "diffs recursively" $
      diff dummyHorse dummyHorse2 `shouldBe`
      objFromList [(pack "r", objFromList [(pack "0",
        objFromList [(pack "m", objFromList [(pack "0", String $ pack "blue")])]
      )])]

    it "handles changes in arrays" $
      diff arr1 arr2
      `shouldBe` objFromList [(pack "m", objFromList [(pack "1", Number 4)])]

    it "handles changes in nested arrays" $
      diff nestedArr1 nestedArr2 `shouldBe`
      objFromList [(pack "r", objFromList [(pack "0",
        objFromList [(pack "m", objFromList [(pack "1", Number 4)])]
      )])]

  describe "patch" $ do
    it "handles modified field" $
      patch dummyUser (diff dummyUser dummyUser7) `shouldBe` dummyUser7

    it "handles deleted fields" $
      patch dummyUser (diff dummyUser dummyUser6) `shouldBe` dummyUser6

    it "handles added fields" $
      patch dummyUser (diff dummyUser dummyUser2) `shouldBe` dummyUser2
