module Dffptch.Internal where

import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as Text
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

emptyObject = H.empty

emptyDelta = Delta emptyObject emptyObject [] emptyObject

toSortedList :: Object -> [(Text.Text, Value)]
toSortedList = sortOn fst . H.toList

arrToObj :: Array -> Value
arrToObj = Object . H.fromList . zip (map (Text.pack . show) [0..]) . V.toList

idxToText :: Int -> Text.Text
idxToText = Text.singleton . Char.chr . (+48)

addAdd :: Delta -> Text.Text -> Value -> Delta
addAdd d k v = d { adds = H.insert k v (adds d) }

addRm :: Delta -> Int -> Delta
addRm d idx = d { dels = (fromIntegral idx) : (dels d) }

addChange :: Delta -> (Delta -> Object) -> Int -> Value -> Object
addChange d field idx val = H.insert (idxToText idx) val (field d)

addMod :: Delta -> Int -> Value -> Delta
addMod d idx v = d { mods = (addChange d mods idx v) }

addRec :: Delta -> Int -> Value -> Delta
addRec d idx v = d { recurses = (addChange d recurses idx v) }

findMod :: Delta -> Int -> Text.Text -> Value -> Value -> Delta
findMod delta idx key (Object aObj) (Object bObj) =
  if aObj == bObj then delta else addRec delta idx recursiveDelta
  where recursiveDelta = Object . deltaToObject $ findChange (H.toList aObj) (H.toList bObj)
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

findChange :: [(Text.Text, Value)] -> [(Text.Text, Value)] -> Delta
findChange as bs = snd $ mergeWith (comparing fst) fa fb fab (0, emptyDelta) as bs where
  fa (aIdx, delta)  (aKey,aVal)               = (aIdx + 1, addRm delta aIdx)
  fb (aIdx, delta)               (bKey, bVal) = (aIdx    , addAdd delta bKey bVal)
  fab (aIdx, delta) (aKey, aVal) (bKey, bVal) = (aIdx + 1, findMod delta aIdx aKey aVal bVal)

addIf :: Char -> (Delta -> Object) -> Delta -> Object -> Object
addIf name prop delta diff =
  if prop delta /= emptyObject
    then H.insert (Text.singleton name) (Object $ prop delta) diff
    else diff

extractDels :: Delta -> Object -> Object
extractDels delta diff =
  if not . null $ dels delta then H.insert (Text.pack "d") (toNumArr delta) diff else diff
  where toNumArr = Array . V.fromList . map Number . dels

deltaToObject :: Delta -> Object
deltaToObject delta = extract H.empty
  where extract = mconcat $ map ($ delta) [extractDels, addIf 'r' recurses, addIf 'a' adds, addIf 'm' mods]

diff :: Value -> Value -> Value
diff (Array old) (Array new) = diff (arrToObj old) (arrToObj new)
diff (Object old) (Object new) = Object . deltaToObject $ findChange o n
  where o = toSortedList old
        n = toSortedList new
diff old new = new

keyToIdx :: Text.Text -> Int
keyToIdx = (subtract 48) . Char.ord . Text.head

createKeys :: Object -> V.Vector Text.Text
createKeys = V.fromList . sort . H.keys

valToInt :: Value -> Maybe Int
valToInt (Number scient) = Scientific.toBoundedInteger scient

handleAdds :: Object -> Value -> Object
handleAdds obj (Object adds) = adds `H.union` obj

handleMods_ :: Object -> V.Vector Text.Text -> [(Text.Text, Value)] -> Object
handleMods_ obj _ [] = obj
handleMods_ obj keys ((key,val):mods) = handleMods_ newObj keys mods
  where newObj = H.insert (keys V.! keyToIdx key) val obj

handleMods :: Object -> Value -> Object
handleMods obj (Object mods) = handleMods_ obj (createKeys obj) $ H.toList mods

handleDels :: Object -> Value -> Object
handleDels obj (Array dels) = foldr H.delete obj deleted
  where keys = (createKeys obj)
        deleted = map (keys V.!) . catMaybes . map valToInt . V.toList $ dels

handleRecs_ :: Object -> V.Vector Text.Text -> [(Text.Text, Value)] -> Object
handleRecs_ obj _ [] = obj
handleRecs_ obj keys ((abr,delta):recs) = handleRecs_ newObj keys recs
  where key = keys V.! keyToIdx abr
        newObj = H.adjust (flip patch delta) key obj

handleRecs :: Object -> Value -> Object
handleRecs obj (Object recs) = handleRecs_ obj (createKeys obj) $ H.toList recs

fields = map Text.pack ["a", "m", "d", "r"]
handlers = [handleAdds, handleMods, handleDels, handleRecs]

patch :: Value -> Value -> Value
patch (Object obj) (Object delta) = Object . foldr ap obj $ fnsVals
  where fnsVals = zip handlers $ map (flip H.lookup delta) fields
        ap (fn,val) obj = maybe obj (fn obj) val
patch obj delta = obj
