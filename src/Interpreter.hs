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
abort = mzero

onStm :: Statement -> Interpreter () -> Interpreter ()
onStm statement (Interpreter f) = (Interpreter $ \ state@(State _ code _ _ position _) ->
  if isStatement position code statement then f state else Nothing) >> continue

onMovementStm :: (Movement -> Interpreter ()) -> Interpreter ()
onMovementStm f = (Interpreter $ \ state@(State _ code _ _ position _) ->
  case getStatement position code of
    MovementStm movement -> let (Interpreter g) = f movement in g state
    _ -> Nothing) >> continue

onValueStm :: (Value -> Interpreter ()) -> Interpreter ()
onValueStm f = (Interpreter $ \ state@(State _ code _ _ position _) ->
  case getStatement position code of
    ValueStm value -> let (Interpreter g) = f value in g state
    _ -> Nothing) >> continue

onToggleReadStm :: (Maybe Char -> Interpreter ()) -> Interpreter ()
onToggleReadStm f = Interpreter $ \ state@(State _ code _ _ position _) ->
  case get position code of
    (_, ToggleReadStm) -> let (Interpreter g) = (f Nothing) in g state
    (character, _) -> let (Interpreter g) = (f $ Just character) in g state

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

emptyInterpreter :: Interpreter ()
emptyInterpreter = onStm EmptyStm $
  return ()

valueInterpreter :: Interpreter ()
valueInterpreter = onValueStm $ \ value -> do
  doPush value

movementInterpreter :: Interpreter ()
movementInterpreter = onMovementStm $ \ movement -> do
  doMove movement

randomMovementInterpreter :: Interpreter ()
randomMovementInterpreter = onStm RandomMovementStm $
  doRandomMove

endInterpreter :: Interpreter ()
endInterpreter = onStm EndStm $
  abort

popAndPrintValueInterpreter :: Interpreter ()
popAndPrintValueInterpreter = onStm PopAndPrintValueStm $ do
  value <- doPop
  doPrintAsInt value

popAndPrintCharInterpreter :: Interpreter ()
popAndPrintCharInterpreter = onStm PopAndPrintCharStm $ do
  value <- doPop
  doPrintAsChar value

popAndDiscardInterpreter :: Interpreter ()
popAndDiscardInterpreter = onStm PopAndDiscardStm $ do
  _ <- doPop
  return ()

popAndMoveHorizontalInterpreter :: Interpreter ()
popAndMoveHorizontalInterpreter = onStm PopAndMoveHorizontalStm $ do
  value <- doPop
  doMove $ if value == 0 then RightMvm else LeftMvm

popAndMoveVerticalInterpreter :: Interpreter ()
popAndMoveVerticalInterpreter = onStm PopAndMoveVerticalStm $ do
  value <- doPop
  doMove $ if value == 0 then DownMvm else UpMvm

duplicateInterpreter :: Interpreter ()
duplicateInterpreter = onStm DuplicateStm $ do
  value <- doPeekOrElse 0
  doPush value

swapInterpreter :: Interpreter ()
swapInterpreter = onStm SwapStm $ do
  value <- doPop
  value' <- doPeekOrElse 0
  doPush value
  doPush value'

addInterpreter :: Interpreter ()
addInterpreter = onStm AddStm $ do
  value <- doPop
  value' <- doPop
  doPush $ value + value'

subInterpreter :: Interpreter ()
subInterpreter = onStm SubStm $ do
  value <- doPop
  value' <- doPop
  doPush $ value - value'

mulInterpreter :: Interpreter ()
mulInterpreter = onStm MulStm $ do
  value <- doPop
  value' <- doPop
  doPush $ value * value'

divInterpreter :: Interpreter ()
divInterpreter = onStm DivStm $ do
  value <- doPop
  value' <- doPop
  doPush $ value `div` value'

modInterpreter :: Interpreter ()
modInterpreter = onStm ModStm $ do
  value <- doPop
  value' <- doPop
  doPush $ value `mod` value'

greaterInterpreter :: Interpreter ()
greaterInterpreter = onStm GreaterStm $ do
  value <- doPop
  value' <- doPop
  doPush $ if value' > value then 1 else 0

notInterpreter :: Interpreter ()
notInterpreter = onStm NotStm $ do
  value <- doPop
  doPush $ if value == 0 then 1 else 0

skipInterpreter :: Interpreter ()
skipInterpreter = onStm SkipStm $ do
  return ()

getInterpreter :: Interpreter ()
getInterpreter = onStm GetStm $ do
  value <- doPop
  value' <- doPop
  character <- doGetCharacter (value', value)
  doPush $ Char.ord character

setInterpreter :: Interpreter ()
setInterpreter = onStm SetStm $ do
  value <- doPop
  value' <- doPop
  value'' <- doPop
  let character = Char.chr value''
  let statement = parseStatement character
  doSet (value', value) (character, statement)

toggleInterpreter :: Interpreter ()
toggleInterpreter = onStm ToggleReadStm $ do
  continue
  readInterpreter

readInterpreter :: Interpreter ()
readInterpreter = onToggleReadStm $ \ mCharacter -> do
  case mCharacter of
    Nothing ->
      return ()
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
