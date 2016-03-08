module Parser (
  parse, parseStatement
) where

import qualified Data.Char as Char (isDigit, digitToInt, ord)

import Types (Movement(..), Position, Statement(..))
import Code (Code, fromList)

splitString :: String -> [String]
splitString "" = []
splitString s = cs : splitString (dropWhile ('\n' ==) s')
  where
    (cs, s') = break ('\n' ==) s

scanChars :: [String] -> [(Position, Char)]
scanChars cs = [((x, y), c) | (y, xcs) <- zip [0 ..] $ map (zip [0 ..]) cs, (x, c) <- xcs]

parseStatement :: Char -> Statement
parseStatement c
  | c == ' '       = EmptyStm
  | c == '<'       = MovementStm LeftMvm
  | c == '>'       = MovementStm RightMvm
  | c == '^'       = MovementStm UpMvm
  | c == 'v'       = MovementStm DownMvm
  | c == '?'       = RandomMovementStm
  | c == '@'       = EndStm
  | c == '.'       = PopAndPrintValueStm
  | c == ','       = PopAndPrintCharStm
  | c == '$'       = PopAndDiscardStm
  | c == '_'       = PopAndMoveHorizontalStm
  | c == '|'       = PopAndMoveVerticalStm
  | c == ':'       = DuplicateStm
  | c == '\\'      = SwapStm
  | c == '+'       = AddStm
  | c == '-'       = SubStm
  | c == '*'       = MulStm
  | c == '/'       = DivStm
  | c == '%'       = ModStm
  | c == '`'       = GreaterStm
  | c == '!'       = NotStm
  | c == '#'       = SkipStm
  | c == 'g'       = GetStm
  | c == 'p'       = SetStm
  | c == '\"'      = ToggleReadStm
  | Char.isDigit c = ValueStm $ Char.digitToInt c
  | otherwise      = ValueStm $ Char.ord c

parse :: String -> Code
parse = fromList . map (\(p, c) -> (p, (c, parseStatement c))) . scanChars . splitString
