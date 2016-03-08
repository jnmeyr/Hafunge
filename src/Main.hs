module Main (
  main
) where

import System.Environment (getArgs)
import System.Random (getStdGen)

import Parser (parse)
import Interpreter (interpret)

helloWorld :: String
helloWorld = ">25*\"!dlroW olleH\":v\n                v:,_@\n                >  ^"

main :: IO ()
main = do
  args <- getArgs
  let source = if length args == 1 then head args else helloWorld
  generator <- getStdGen
  print $ interpret generator $ parse source
