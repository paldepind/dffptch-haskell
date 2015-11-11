{-# LANGUAGE OverloadedStrings #-}

module Dffptch.Internal where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Scientific as S
import qualified Data.Char as Char
import Data.Ord
import Debug.Trace
import Control.Applicative

data Delta = Delta
    { adds     :: Object -- Added keys
    , mods     :: Object -- Modified fields
    , dels     :: [Scientific] -- Deleted fields
    , recurses :: H.HashMap Text Delta -- Recursively change fields
    } deriving (Show, Eq)

emptyDelta = Delta H.empty H.empty [] H.empty

notEmptyVal v = v /= emptyArray && v /= emptyObject

instance ToJSON Delta where
  toJSON (Delta adds mods dels recs) =
    object $ filter (notEmptyVal . snd) ["a" .= adds, "m" .= mods, "d" .= dels, "r" .= recs]

defaultToEmpty :: (FromJSON a, Monoid a) => Object -> Text -> Parser a
defaultToEmpty o key = o .:? key .!= mempty

instance FromJSON Delta where
  parseJSON = withObject "delta" $ \d ->
    Delta <$> defaultToEmpty d "a"
          <*> defaultToEmpty d "m"
          <*> defaultToEmpty d "d"
          <*> defaultToEmpty d "r"

toSortedList :: Object -> [(Text, Value)]
toSortedList = sortOn fst . H.toList

toAssocList :: Array -> [(Text, Value)]
toAssocList = zip (map (T.pack . show) [0..]) . V.toList

arrToObj :: Array -> Value
arrToObj = Object . H.fromList . zip (map (T.pack . show) [0..]) . V.toList

objToArr :: Object -> Value
objToArr = Array . V.fromList . map snd . sortBy (comparing fst) . H.toList

idxToText :: Int -> Text
idxToText = T.singleton . Char.chr . (+48)

addAdd :: Delta -> Text -> Value -> Delta
addAdd d k v = d { adds = H.insert k v (adds d) }

addRm :: Delta -> Int -> Delta
addRm d idx = d { dels = fromIntegral idx : dels d }

addChange :: Delta -> (Delta -> Object) -> Int -> Value -> Object
addChange d field idx val = H.insert (idxToText idx) val (field d)

addMod :: Delta -> Int -> Value -> Delta
addMod d idx v = d { mods = addChange d mods idx v }

addRec :: Delta -> Int -> Delta -> Delta
addRec d idx v = d { recurses = H.insert (idxToText idx) v (recurses d) }

findMod :: Delta -> Int -> Text -> Value -> Value -> Delta
findMod delta idx key (Object aObj) (Object bObj) =
  if aObj == bObj then delta else addRec delta idx recursiveDelta
  where recursiveDelta = findChange (H.toList aObj) (H.toList bObj)
findMod delta idx key (Array aArr) (Array bArr) =
  findMod delta idx key (arrToObj aArr) (arrToObj bArr)
findMod delta idx key aVal bVal =
  if aVal == bVal then delta else addMod delta idx bVal

mergeWith :: (a -> b -> Ordering) -> (c -> a -> c) -> (c -> b -> c) -> (c -> a -> b -> c) -> c -> [a] -> [b] -> c
mergeWith comparer fa fb fab = go where
  go acc as [] = foldl fa acc as
  go acc [] bs = foldl fb acc bs
  go acc ass@(a:as) bss@(b:bs) = case comparer a b of
    EQ -> go (fab acc a b) as  bs
    LT -> go (fa acc a   ) as  bss
    GT -> go (fb acc    b) ass bs

findChange :: [(Text, Value)] -> [(Text, Value)] -> Delta
findChange as bs = snd $ mergeWith (comparing fst) fa fb fab (0, emptyDelta) as bs where
  fa  (aIdx, delta) (aKey, aVal)              = (aIdx + 1, addRm delta aIdx)
  fb  (aIdx, delta)              (bKey, bVal) = (aIdx    , addAdd delta bKey bVal)
  fab (aIdx, delta) (aKey, aVal) (bKey, bVal) = (aIdx + 1, findMod delta aIdx aKey aVal bVal)

diff :: Value -> Value -> Delta
diff (Array old) (Array new) = findChange (toAssocList old) (toAssocList new)
diff (Object old) (Object new) = findChange (toSortedList old) (toSortedList new)

keyToIdx :: Text -> Int
keyToIdx = subtract 48 . Char.ord . T.head

getKeys :: Object -> V.Vector Text
getKeys = V.fromList . sort . H.keys

getKeyIdxs = fmap keyToIdx . getKeys

handleAdds :: Object -> Object -> Object
handleAdds adds obj = adds `H.union` obj

handleMods :: Object -> Object -> Object
handleMods mods obj = H.foldrWithKey go obj mods
  where keys = getKeys obj
        go key = H.insert (keys V.! keyToIdx key)

handleDels :: [Scientific] -> Object -> Object
handleDels dels obj = foldr H.delete obj deleted
  where keys = getKeys obj
        deleted = map (keys V.!) . mapMaybe S.toBoundedInteger $ dels

handleRecs_ :: Object -> V.Vector Text -> [(Text, Delta)] -> Object
handleRecs_ obj _ [] = obj
handleRecs_ obj keys ((abr,delta):recs) = handleRecs_ newObj keys recs
  where key = keys V.! keyToIdx abr
        newObj = H.adjust (`patch` delta) key obj

handleRecs :: H.HashMap Text Delta -> Object -> Object
handleRecs recs obj = handleRecs_ obj (getKeys obj) $ H.toList recs

patch :: Value -> Delta -> Value
patch (Object obj) delta = Object $ handleAdds (adds delta) .
                                    handleMods (mods delta) .
                                    handleDels (dels delta) .
                                    handleRecs (recurses delta) $ obj
patch (Array list) delta =
  let Object obj = patch (arrToObj list) delta
  in objToArr obj
