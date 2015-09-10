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

findChange :: Delta -> Int -> [(Text, Value)] -> [(Text, Value)] -> Delta
findChange delta _ [] [] = delta
findChange delta aIdx [] ((bKey,bVal):bs) = findChange (addAdd delta bKey bVal) aIdx [] bs
findChange delta aIdx a@((aKey,aVal):as) [] = findChange (addRm delta aIdx) (aIdx+1) as []
findChange delta aIdx a@((aKey,aVal):as) b@((bKey,bVal):bs) =
  case compare aKey bKey of
    EQ -> findChange (addMod delta aIdx aKey aVal bVal) (aIdx+1) as bs
    --EQ -> findChange delta (aIdx+1) as bs
    LT -> findChange (addRm delta aIdx) (aIdx+1) as b
    GT -> findChange (addAdd delta bKey bVal) aIdx a bs

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
diff (Object old) (Object new) = Object . deltaToObject $ findChange emptyDelta 0 o n
  where o = toSortedList old
        n = toSortedList new
diff old new = new

patch :: Value -> Value -> Value
patch obj delta = obj
