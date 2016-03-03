module Output (
  printAsInt, printAsChar
) where

import qualified Data.Char as Char (chr)

import Types (Output, Value)

printAsInt :: Value -> Output -> Output
printAsInt value output = output ++ show value

printAsChar :: Value -> Output -> Output
printAsChar value output = output ++ [Char.chr value]
