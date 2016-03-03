module Types (
  Movement(..),
  Output,
  Position,
  Statement(..),
  Status,
  Value
) where

data Movement =
  LeftMvm |
  RightMvm |
  UpMvm |
  DownMvm

type Output = String

type Position = (Int, Int)

data Statement =
  EmptyStm |
  ValueStm Value |
  MovementStm Movement |
  RandomMovementStm |
  EndStm |
  PopAndPrintValueStm |
  PopAndPrintAsciiStm |
  PopAndDiscardStm |
  PopAndMoveHorStm |
  PopAndMoveVerStm |
  DuplicateStm |
  SwapStm |
  AddStm |
  SubStm |
  MulStm |
  DivStm |
  ModStm |
  GreaterStm |
  NotStm |
  SkipStm |
  GetStm |
  SetStm |
  ToggleReadStm |
  ReadStm

type Status = Bool

type Value = Int
