module Dffptch.Internal where

import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Text (Text, pack, singleton)
import Data.List
import Data.Scientific as Scientific
import Data.Char (chr)
import Data.Ord

data Delta = Delta
    { adds     :: Object -- Added keys
    , mods     :: Object -- Modified fields
    , dels     :: [Scientific] -- Deleted fields
    , recurses :: Object -- Recursively change fields
    } deriving (Show)

emptyObject = H.empty

emptyDelta = Delta emptyObject emptyObject [] emptyObject

toSortedList :: Object -> [(Text, Value)]
toSortedList = sortOn fst . H.toList


addAdd :: Delta -> Text -> Value -> Delta
addAdd d k v = d { adds = H.insert k v (adds d) }

addRm :: Delta -> Int -> Delta
addRm d idx = d { dels = (fromIntegral idx) : (dels d) }

addMod_ :: Delta -> Int -> Value -> Delta
addMod_ d idx v = d { mods = H.insert char v (mods d) }
  where char = singleton . chr $ 48 + idx

addMod :: Delta -> Int -> Text -> Value -> Value -> Delta
addMod delta idx key aVal bVal =
  if aVal == bVal
    then delta
    else addMod_ delta idx bVal

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
  fa (aIdx, delta)  (aKey,aVal)               = (aIdx + 1, addRm delta aIdx)
  fb (aIdx, delta)               (bKey, bVal) = (aIdx    , addAdd delta bKey bVal)
  fab (aIdx, delta) (aKey, aVal) (bKey, bVal) = (aIdx + 1, addMod delta aIdx aKey aVal bVal)

addIf :: Delta -> Char -> (Delta -> Object) -> Object -> Object
addIf delta name prop diff =
  if prop delta /= emptyObject
    then H.insert (singleton name) (Object $ prop delta) diff
    else diff

extractDels :: Delta -> Object -> Object
extractDels delta diff =
  if dels delta /= []
    then H.insert (pack "d") (Array . V.fromList . map Number . dels $ delta) diff
    else diff

deltaToObject :: Delta -> Object
deltaToObject delta = extractDels delta . addIf delta 'a' adds . addIf delta 'm' mods $ H.empty

diff :: Value -> Value -> Value
diff (Array old) (Array new) = Array old
-- diff (Object old) (Object new) = Object . deltaToObject $ findChange emptyDelta 0 o n
diff (Object old) (Object new) = Object . deltaToObject $ findChange o n
  where o = toSortedList old
        n = toSortedList new
diff old new = new

patch :: Value -> Value -> Value
patch obj delta = obj
