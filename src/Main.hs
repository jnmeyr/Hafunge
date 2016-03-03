module Main (
  main
) where

import Prelude hiding (getChar)

import System.Random (StdGen, getStdGen, randomR)

import qualified Data.Char as Char (chr, ord)

import Types (Movement(..), Output, Position, Statement(..), Status, Value)
import Stack (Stack, push, peekOrElse, pop, popOrElse)
import Code (Code, get, getChar, getStm, set)
import Parser (parse, parseStatement)
import Input
import Output (printAsInt, printAsChar)

data State = State StdGen Code (Stack Value) Movement Position Output Status

randomMovement :: StdGen -> (StdGen, Movement)
randomMovement generator = (generator', movement)
  where
    (n, generator') = randomR (0, 3) generator :: (Int, StdGen)
    movement = case n of
      0 -> LeftMvm
      1 -> RightMvm
      2 -> DownMvm
      3 -> UpMvm
      _ -> error "randomMovement: this should never happen"

move :: Movement -> Position -> Position
move movement (x, y) = case movement of
  LeftMvm -> (x - 1, y)
  RightMvm -> (x + 1, y)
  UpMvm -> (x, y - 1)
  DownMvm -> (x, y + 1)

execute :: Statement -> State -> State
execute EmptyStm state = state
execute (ValueStm value) (State generator code stack movement position output status) = State generator code stack' movement position output status
  where
    stack' = push value stack
execute (MovementStm movement) (State generator code stack _ position output status) = State generator code stack movement position output status
execute RandomMovementStm (State generator code stack _ position output status) = State generator' code stack movement position output status
  where
    (generator', movement) = randomMovement generator
execute EndStm (State generator code stack movement position output _) = State generator code stack movement position output False
execute PopAndPrintValueStm (State generator code stack movement position output status) = State generator code stack' movement position output' status
  where
    (stack', value) = pop stack
    output' = printAsInt value output
execute PopAndPrintAsciiStm (State generator code stack movement position output status) = State generator code stack' movement position output' status
  where
    (stack', value) = pop stack
    output' = printAsChar value output
execute PopAndDiscardStm (State generator code stack movement position output status) = State generator code stack' movement position output status
  where
    (stack', _) = pop stack
execute PopAndMoveHorStm (State generator code stack _ position output status) = State generator code stack' movement position output status
  where
    (stack', value) = pop stack
    movement = if value == 0 then RightMvm else LeftMvm
execute PopAndMoveVerStm (State generator code stack _ position output status) = State generator code stack' movement position output status
  where
    (stack', value) = pop stack
    movement = if value == 0 then DownMvm else UpMvm
execute DuplicateStm (State generator code stack movement position output status) = State generator code stack'' movement position output status
  where
    (stack', value) = peekOrElse stack 0
    stack'' = push value stack'
execute SwapStm (State generator code stack movement position output status) = State generator code stack'''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = popOrElse stack' 0
    stack''' = push value stack''
    stack'''' = push value' stack'''
execute AddStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    value'' = value + value'
    stack''' = push value'' stack''
execute SubStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    value'' = value' - value
    stack''' = push value'' stack''
execute MulStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    value'' = value * value'
    stack''' = push value'' stack''
execute DivStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    value'' = if value /= 0 then value' `div` value else 0
    stack''' = push value'' stack''
execute ModStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    value'' = if value /= 0 then value' `mod` value else 0
    stack''' = push value'' stack''
execute GreaterStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    value'' = if value' > value then 1 else 0
    stack''' = push value'' stack''
execute NotStm (State generator code stack movement position output status) = State generator code stack'' movement position output status
  where
    (stack', value) = pop stack
    value' = if value == 0 then 1 else 0
    stack'' = push value' stack'
execute SkipStm (State generator code stack movement position output status) = State generator code stack movement position' output status
  where
    position' = move movement position
execute GetStm (State generator code stack movement position output status) = State generator code stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    char = getChar (value', value) code
    value'' = Char.ord char
    stack''' = push value'' stack''
execute SetStm (State generator code stack movement position output status) = State generator code' stack''' movement position output status
  where
    (stack', value) = pop stack
    (stack'', value') = pop stack'
    (stack''', value'') = pop stack''
    char = Char.chr value''
    stm = parseStatement char
    code' = set (value', value) (char, stm) code
execute ToggleReadStm (State generator code stack movement position output status) = state
  where
    position' = move movement position
    state = execute ReadStm (State generator code stack movement position' output status)
execute ReadStm (State generator code stack movement position output status) = case statement of
  ToggleReadStm ->
    State generator code stack movement position output status
  _ ->
    execute ReadStm (State generator code stack' movement position' output status)
      where
        value = Char.ord char
        stack' = push value stack
        position' = move movement position
  where
    (char, statement) = get position code

step :: State -> State
step state@(State _ code _ _ position _ _) = State generator code' stack movement position'' output status
  where
    statement = getStm position code
    (State generator code' stack movement position' output status) = execute statement state
    position'' = move movement position'

run :: State -> State
run state@(State _ _ _ _ _ _ False) = state
run state = run $ step state

interpret :: StdGen -> String -> String
interpret generator input = output
  where
    (State _ _ _ _ _ output _) = run $ State generator (parse input) [] RightMvm (0, 0) "" True

main :: IO ()
main = do
  generator <- getStdGen
  print $ interpret generator demo
