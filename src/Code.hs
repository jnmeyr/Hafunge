module Code (
  Code,
  fromList, toList,
  get, getCharacter, getStatement, isStatement,
  set
) where

import Prelude hiding (getChar)

import Data.Map (Map)
import qualified Data.Map as Map (fromList, toList, lookup, insert)
import qualified Data.Maybe as Maybe (fromJust)

import Types (Position, Statement)

type Code = Map Position (Char, Statement)

fromList :: [(Position, (Char, Statement))] -> Code
fromList = Map.fromList

toList :: Code -> [(Position, (Char, Statement))]
toList = Map.toList

get :: Position -> Code -> (Char, Statement)
get position code = Maybe.fromJust $ Map.lookup position code

getCharacter :: Position -> Code -> Char
getCharacter position code = fst $ get position code

getStatement :: Position -> Code -> Statement
getStatement position code = snd $ get position code

isStatement :: Position -> Code -> Statement -> Bool
isStatement position code statement = statement == snd (get position code)

set :: Position -> (Char, Statement) -> Code -> Code
set = Map.insert
