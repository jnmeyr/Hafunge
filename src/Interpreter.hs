module Interpreter (
  interpret
) where

import Prelude hiding (getChar)

import Control.Applicative
import Control.Monad

import System.Random (StdGen)

import qualified Data.Char as Char (chr, ord)

import Types (Movement(..), randomMovement, Output, Position, Statement(..), Value)
import Stack (Stack, push, peekOrElse, pop)
import Code (Code, get, getCharacter, getStatement, isStatement, set)
import Parser (parseStatement)
import Output (printAsInt, printAsChar)

data State = State StdGen Code (Stack Value) Movement Position Output

newtype Interpreter a = Interpreter { runInterpreter :: State -> Maybe (a, State) }

instance Functor Interpreter where
  fmap f (Interpreter g) = Interpreter $ \ state ->
    case g state of
      Nothing ->
        Nothing
      Just (a, state') ->
        Just (f a, state')

instance Applicative Interpreter where
  pure = return
  (<*>) = ap

instance Alternative Interpreter where
  empty = Interpreter $ const
    Nothing
  (<|>) (Interpreter f) (Interpreter g) = Interpreter $ \ state ->
    case f state of
      Nothing ->
        g state
      res ->
        res

instance Monad Interpreter where
  return a = Interpreter $ \ state ->
    Just (a, state)
  (>>=) (Interpreter g) f = Interpreter $ \ state ->
    case g state of
      Nothing ->
        Nothing
      Just (a, state') ->
        let (Interpreter h) = f a in h state'

instance MonadPlus Interpreter where
  mzero = empty
  mplus = (<|>)

requireStm :: Statement -> Interpreter ()
requireStm statement = Interpreter $ \ state@(State _ code _ _ position _) ->
  if isStatement position code statement then Just ((), state) else Nothing

requireMovementStm :: Interpreter Movement
requireMovementStm = Interpreter $ \ state@(State _ code _ _ position _) ->
  case getStatement position code of
    MovementStm movement -> Just (movement, state)
    _ -> Nothing

requireValueStm :: Interpreter Value
requireValueStm = Interpreter $ \ state@(State _ code _ _ position _) ->
  case getStatement position code of
    ValueStm value -> Just (value, state)
    _ -> Nothing

requireToggleReadStm :: Interpreter (Maybe Char)
requireToggleReadStm = Interpreter $ \ state@(State _ code _ _ position _) ->
  case get position code of
    (_, ToggleReadStm) -> Just (Nothing, state)
    (character, _) -> Just (Just character, state)

doMove :: Movement -> Interpreter ()
doMove movement = Interpreter $ \ (State generator code stack _ position output) ->
  Just ((), State generator code stack movement position output)

doRandomMove :: Interpreter ()
doRandomMove = Interpreter $ \ (State generator code stack _ position output) ->
  let (movement, generator') = randomMovement generator in Just ((), State generator' code stack movement position output)

doPop :: Interpreter Value
doPop = Interpreter $ \ (State generator code stack movement position output) ->
  let (stack', value) = pop stack in Just (value, State generator code stack' movement position output)

doPush :: Value -> Interpreter ()
doPush value = Interpreter $ \ (State generator code stack movement position output) ->
  let stack' = push value stack in Just ((), State generator code stack' movement position output)

doPeekOrElse :: Value -> Interpreter Value
doPeekOrElse value = Interpreter $ \ (State generator code stack movement position output) ->
  let (stack', value') = peekOrElse stack value in Just (value', State generator code stack' movement position output)

doPrintAsInt :: Value -> Interpreter ()
doPrintAsInt value = Interpreter $ \ (State generator code stack movement position output) ->
  let output' = printAsInt value output in Just ((), State generator code stack movement position output')

doPrintAsChar :: Value -> Interpreter ()
doPrintAsChar value = Interpreter $ \ (State generator code stack movement position output) ->
  let output' = printAsChar value output in Just ((), State generator code stack movement position output')

doGetCharacter :: Position -> Interpreter Char
doGetCharacter position = Interpreter $ \ (State generator code stack movement position' output) ->
  let character = getCharacter position code in Just (character, State generator code stack movement position' output)

doSet :: Position -> (Char, Statement) -> Interpreter ()
doSet position (character, statement) = Interpreter $ \ (State generator code stack movement position' output) ->
  let code' = set position (character, statement) code in Just ((), State generator code' stack movement position' output)

continue :: Interpreter ()
continue = Interpreter $ \ (State generator code stack movement (x, y) output) ->
  let
    position = case movement of
      LeftMvm -> (x - 1, y)
      RightMvm -> (x + 1, y)
      UpMvm -> (x, y - 1)
      DownMvm -> (x, y + 1)
  in
    Just ((), State generator code stack movement position output)

abort :: Interpreter ()
abort = empty

emptyInterpreter :: Interpreter ()
emptyInterpreter = do
  requireStm EmptyStm
  continue

valueInterpreter :: Interpreter ()
valueInterpreter = do
  value <- requireValueStm
  doPush value
  continue

movementInterpreter :: Interpreter ()
movementInterpreter = do
  movement <- requireMovementStm
  doMove movement
  continue

randomMovementInterpreter :: Interpreter ()
randomMovementInterpreter = do
  requireStm RandomMovementStm
  doRandomMove
  continue

endInterpreter :: Interpreter ()
endInterpreter = do
  requireStm EndStm
  abort

popAndPrintValueInterpreter :: Interpreter ()
popAndPrintValueInterpreter = do
  requireStm PopAndPrintValueStm
  value <- doPop
  doPrintAsInt value
  continue

popAndPrintCharInterpreter :: Interpreter ()
popAndPrintCharInterpreter = do
  requireStm PopAndPrintCharStm
  value <- doPop
  doPrintAsChar value
  continue

popAndDiscardInterpreter :: Interpreter ()
popAndDiscardInterpreter = do
  requireStm PopAndDiscardStm
  _ <- doPop
  continue

popAndMoveHorizontalInterpreter :: Interpreter ()
popAndMoveHorizontalInterpreter = do
  requireStm PopAndMoveHorizontalStm
  value <- doPop
  doMove $ if value == 0 then RightMvm else LeftMvm
  continue

popAndMoveVerticalInterpreter :: Interpreter ()
popAndMoveVerticalInterpreter = do
  requireStm PopAndMoveVerticalStm
  value <- doPop
  doMove $ if value == 0 then DownMvm else UpMvm
  continue

duplicateInterpreter :: Interpreter ()
duplicateInterpreter = do
  requireStm DuplicateStm
  value <- doPeekOrElse 0
  doPush value
  continue

swapInterpreter :: Interpreter ()
swapInterpreter = do
  requireStm SwapStm
  value <- doPop
  value' <- doPeekOrElse 0
  doPush value
  doPush value'
  continue

addInterpreter :: Interpreter ()
addInterpreter = do
  requireStm AddStm
  value <- doPop
  value' <- doPop
  doPush $ value + value'
  continue

subInterpreter :: Interpreter ()
subInterpreter = do
  requireStm SubStm
  value <- doPop
  value' <- doPop
  doPush $ value - value'
  continue

mulInterpreter :: Interpreter ()
mulInterpreter = do
  requireStm MulStm
  value <- doPop
  value' <- doPop
  doPush $ value * value'
  continue

divInterpreter :: Interpreter ()
divInterpreter = do
  requireStm DivStm
  value <- doPop
  value' <- doPop
  doPush $ value `div` value'
  continue

modInterpreter :: Interpreter ()
modInterpreter = do
  requireStm ModStm
  value <- doPop
  value' <- doPop
  doPush $ value `mod` value'
  continue

greaterInterpreter :: Interpreter ()
greaterInterpreter = do
  requireStm GreaterStm
  value <- doPop
  value' <- doPop
  doPush $ if value' > value then 1 else 0
  continue

notInterpreter :: Interpreter ()
notInterpreter = do
  requireStm NotStm
  value <- doPop
  doPush $ if value == 0 then 1 else 0
  continue

skipInterpreter :: Interpreter ()
skipInterpreter = do
  requireStm SkipStm
  continue

getInterpreter :: Interpreter ()
getInterpreter = do
  requireStm GetStm
  value <- doPop
  value' <- doPop
  character <- doGetCharacter (value', value)
  doPush $ Char.ord character
  continue

setInterpreter :: Interpreter ()
setInterpreter = do
  requireStm SetStm
  value <- doPop
  value' <- doPop
  value'' <- doPop
  let character = Char.chr value''
  let statement = parseStatement character
  doSet (value', value) (character, statement)
  continue

toggleInterpreter :: Interpreter ()
toggleInterpreter = do
  requireStm ToggleReadStm
  continue
  readInterpreter

readInterpreter :: Interpreter ()
readInterpreter = do
  mCharacter <- requireToggleReadStm
  case mCharacter of
    Nothing ->
      continue
    (Just character) -> do
      doPush $ Char.ord character
      continue
      readInterpreter

interpreter :: Interpreter ()
interpreter =
  emptyInterpreter <|>
  valueInterpreter <|>
  movementInterpreter <|>
  randomMovementInterpreter <|>
  endInterpreter <|>
  popAndPrintValueInterpreter <|>
  popAndPrintCharInterpreter <|>
  popAndDiscardInterpreter <|>
  popAndMoveHorizontalInterpreter <|>
  popAndMoveVerticalInterpreter <|>
  duplicateInterpreter <|>
  swapInterpreter <|>
  addInterpreter <|>
  subInterpreter <|>
  mulInterpreter <|>
  divInterpreter <|>
  modInterpreter <|>
  greaterInterpreter <|>
  notInterpreter <|>
  skipInterpreter <|>
  getInterpreter <|>
  setInterpreter <|>
  toggleInterpreter

interpret :: StdGen -> Code -> Output
interpret generator code = let (State _ _ _ _ _ output) = fixpoint $ State generator code [] RightMvm (0, 0) "" in output
  where
    fixpoint :: State -> State
    fixpoint state = case runInterpreter interpreter state of
      Nothing -> state
      Just ((), state') -> fixpoint state'
