module Parser (
  parse, parseStatement
) where

import qualified Data.Char as Char (isDigit, digitToInt, chr, ord)

import Types (Movement(..), Position, Statement(..))
import Code (Code, fromList)

splitString :: String -> [String]
splitString "" = []
splitString s = cs : splitString (dropWhile ('\n' ==) s')
  where
    (cs, s') = break ('\n' ==) s

scanChars :: [String] -> [(Position, Char)]
scanChars cs = [((x, y), c) | (y, xcs) <- zip [0..] $ map (zip [0..]) cs, (x, c) <- xcs]

parseStatement :: Char -> Statement
parseStatement c
  | c == ' ' = EmptyStm
  | c == '?' = RandomMovementStm
  | c == '@' = EndStm
  | c == '.' = PopAndPrintValueStm
  | c == ',' = PopAndPrintAsciiStm
  | c == '$' = PopAndDiscardStm
  | c == '_' = PopAndMoveHorStm
  | c == '|' = PopAndMoveVerStm
  | c == ':' = DuplicateStm
  | c == '\\' = SwapStm
  | c == '+' = AddStm
  | c == '-' = SubStm
  | c == '*' = MulStm
  | c == '/' = DivStm
  | c == '%' = ModStm
  | c == '`' = GreaterStm
  | c == '!' = NotStm
  | c == '#' = SkipStm
  | c == 'g' = GetStm
  | c == 'p' = SetStm
  | c == '\"' = ToggleReadStm
  | c == '<' = MovementStm LeftMvm
  | c == '>' = MovementStm RightMvm
  | c == '^' = MovementStm UpMvm
  | c == 'v' = MovementStm DownMvm
  | Char.isDigit c = ValueStm $ Char.digitToInt c
  | otherwise = ValueStm $ Char.ord c

parseCode :: [(Position, Char)] -> Code
parseCode = fromList . map (\(p, c) -> (p, (c, parseStatement c)))

parse :: String -> Code
parse = parseCode . scanChars . splitString
