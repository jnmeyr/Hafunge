module Types (
  Movement(..), movement, randomMovement,
  Output,
  Position,
  Statement(..),
  Value
) where

import System.Random (StdGen, randomR)

data Movement =
  LeftMvm |
  RightMvm |
  UpMvm |
  DownMvm

movement :: Int -> Movement
movement n
  | n == 0    = LeftMvm
  | n == 1    = RightMvm
  | n == 2    = UpMvm
  | n == 3    = DownMvm
  | otherwise = error "movement"

randomMovement :: StdGen -> (Movement, StdGen)
randomMovement generator = let (n, generator') = randomR (0, 3) generator in (movement n, generator')

type Output = String

type Position = (Int, Int)

data Statement =
  EmptyStm |
  ValueStm Value |
  MovementStm Movement |
  RandomMovementStm |
  EndStm |
  PopAndPrintValueStm |
  PopAndPrintCharStm |
  PopAndDiscardStm |
  PopAndMoveHorizontalStm |
  PopAndMoveVerticalStm |
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

instance Eq Statement where
  EmptyStm                == EmptyStm                = True
  ValueStm _              == ValueStm _              = True
  MovementStm _           == MovementStm _           = True
  RandomMovementStm       == RandomMovementStm       = True
  EndStm                  == EndStm                  = True
  PopAndPrintValueStm     == PopAndPrintValueStm     = True
  PopAndPrintCharStm      == PopAndPrintCharStm      = True
  PopAndDiscardStm        == PopAndDiscardStm        = True
  PopAndMoveHorizontalStm == PopAndMoveHorizontalStm = True
  PopAndMoveVerticalStm   == PopAndMoveVerticalStm   = True
  DuplicateStm            == DuplicateStm            = True
  SwapStm                 == SwapStm                 = True
  AddStm                  == AddStm                  = True
  SubStm                  == SubStm                  = True
  MulStm                  == MulStm                  = True
  DivStm                  == DivStm                  = True
  ModStm                  == ModStm                  = True
  GreaterStm              == GreaterStm              = True
  NotStm                  == NotStm                  = True
  SkipStm                 == SkipStm                 = True
  GetStm                  == GetStm                  = True
  SetStm                  == SetStm                  = True
  ToggleReadStm           == ToggleReadStm           = True
  ReadStm                 == ReadStm                 = True
  _                       == _                       = False

type Value = Int
