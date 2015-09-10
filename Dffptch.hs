module Dffptch (diff, patch) where

import Data.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as L
import qualified Data.HashMap.Strict as H

import Dffptch.Internal

main :: IO ()
main = do
  rawJSON <- B.readFile "./test.json"
  B.putStr rawJSON
  case decode rawJSON :: Maybe Value of
    Just jsonVal -> B.putStr $ encode (jsonVal)
    Nothing -> error "Parse err"
