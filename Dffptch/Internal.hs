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
import Data.Scientific as Scientific
import qualified Data.Char as Char
import Data.Ord
import Debug.Trace

data Delta = Delta
    { adds     :: Object -- Added keys
    , mods     :: Object -- Modified fields
    , dels     :: [Scientific] -- Deleted fields
    , recurses :: Object -- Recursively change fields
    } deriving (Show)

emptyDelta = Delta H.empty H.empty [] H.empty

notEmptyVal v = v /= emptyArray && v /= emptyObject

instance ToJSON Delta where
  toJSON (Delta adds mods dels recs) =
    object $ filter (notEmptyVal . snd) ["a" .= adds, "m" .= mods, "d" .= dels, "r" .= recs]

toSortedList :: Object -> [(Text, Value)]
toSortedList = sortOn fst . H.toList

toAssocList :: Array -> [(Text, Value)]
toAssocList = zip (map (T.pack . show) [0..]) . V.toList

arrToObj :: Array -> Value
arrToObj = Object . H.fromList . zip (map (T.pack . show) [0..]) . V.toList

objToArr :: Object -> Value
objToArr = Array . V.fromList . map snd . sortBy comp . H.toList
  where comp (t1, _) (t2, _) = compare t1 t2

idxToText :: Int -> Text
idxToText = T.singleton . Char.chr . (+48)

addAdd :: Delta -> Text -> Value -> Delta
addAdd d k v = d { adds = H.insert k v (adds d) }

addRm :: Delta -> Int -> Delta
addRm d idx = d { dels = (fromIntegral idx) : (dels d) }

addChange :: Delta -> (Delta -> Object) -> Int -> Value -> Object
addChange d field idx val = H.insert (idxToText idx) val (field d)

addMod :: Delta -> Int -> Value -> Delta
addMod d idx v = d { mods = (addChange d mods idx v) }

addRec :: Delta -> Int -> Value -> Delta
addRec d idx v = d { recurses = (addChange d recurses idx v) }

findMod :: Delta -> Int -> Text -> Value -> Value -> Delta
findMod delta idx key (Object aObj) (Object bObj) =
  if aObj == bObj then delta else addRec delta idx recursiveDelta
  where recursiveDelta = toJSON $ findChange (H.toList aObj) (H.toList bObj)
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

doDiff old new = toJSON $ findChange old new

diff :: Value -> Value -> Value
diff (Array old) (Array new) = doDiff (toAssocList old) (toAssocList new)
diff (Object old) (Object new) = doDiff (toSortedList old) (toSortedList new)

keyToIdx :: Text -> Int
keyToIdx = (subtract 48) . Char.ord . T.head

getKeys :: Object -> V.Vector Text
getKeys = V.fromList . sort . H.keys

getKeyIdxs = fmap keyToIdx . getKeys

valToInt :: Value -> Maybe Int
valToInt (Number scient) = Scientific.toBoundedInteger scient

handleAdds :: Value -> Object -> Object
handleAdds (Object adds) obj = adds `H.union` obj

handleMods :: Value -> Object -> Object
handleMods (Object mods) obj = H.foldrWithKey go obj mods
  where keys = getKeys obj
        go key val obj = H.insert (keys V.! keyToIdx key) val obj

handleDels :: Value -> Object -> Object
handleDels (Array dels) obj = foldr H.delete obj deleted
  where keys = (getKeys obj)
        deleted = map (keys V.!) . catMaybes . map valToInt . V.toList $ dels

handleRecs_ :: Object -> V.Vector Text -> [(Text, Value)] -> Object
handleRecs_ obj _ [] = obj
handleRecs_ obj keys ((abr,delta):recs) = handleRecs_ newObj keys recs
  where key = keys V.! keyToIdx abr
        newObj = H.adjust (flip patch delta) key obj

handleRecs :: Value -> Object -> Object
handleRecs (Object recs) obj = handleRecs_ obj (getKeys obj) $ H.toList recs

fields = ["a", "m", "d", "r"]
handlers = [handleAdds, handleMods, handleDels, handleRecs]

patch :: Value -> Value -> Value
patch (Object obj) (Object delta) = Object $ foldr (.) id (zipWith fns fields handlers) obj
  where fns field handler = maybe id handler (H.lookup field delta)
patch (Array list) delta =
  let Object obj = patch (arrToObj list) delta
  in objToArr obj
