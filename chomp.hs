-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Chomp player in Haskell

import Data.Array
import Text.Printf

data Move = Move { coords :: (Int, Int) }

newtype Board = Board { squares :: Array (Int, Int) Bool }

rows :: Int
rows = 3
cols :: Int
cols = 4

newBoard :: Board
newBoard = Board $ 
             listArray ((0, 0), (rows - 1, cols - 1)) $ 
             repeat True

instance Show Board where
  show b = unlines $ 
           [ concat $
             [ if squares b ! (i, j) then "o" else "." |
               j <- [0 .. cols - 1] ] |
             i <- [0 .. rows - 1] ]

instance Show Move where
  show move = show (coords move)

getMove :: IO Move
getMove = fmap (Move . read) getLine

makeMove :: Board -> Move -> Board
makeMove b (Move (r0, c0)) =
  Board $
  squares b // [ ((r, c), False) |
                 r <- [r0 .. rows - 1],
                 c <- [c0 .. cols - 1] ]

negamax :: Board -> Either (Maybe Move) Move
negamax b =
  let (0, 0) : sqs = map fst $ filter snd $ assocs $ squares b in
  case findJust $ map tryNegamax sqs of
     Just m -> Right m 
     Nothing -> 
       case sqs of
         [] -> Left Nothing
         _ -> Left $ Just $ Move $ last sqs
  where
    findJust (Just x : _) = Just x
    findJust (_ : xs) = findJust xs
    findJust [] = Nothing
    tryNegamax sq =
      case negamax $ makeMove b $ Move sq of
        Right m -> Just m
        Left _ -> Nothing

type Action = Board -> IO Board

humanTurn :: Action
humanTurn b = 
  fmap (makeMove b) getMove

computerTurn :: Action
computerTurn b = do
  case negamax b of
    Right m -> do
      _ <- printf "%s :-)\n" $ show m
      return $ makeMove b m
    Left (Just m) -> do
      _ <- printf "%s :-P\n" $ show m
      return $ makeMove b m
    Left Nothing ->
      error "internal error: computer move on empty board"

playGame :: Board -> Action -> Action -> IO ()
playGame b a1 a2 = do
  putStr $ show b
  b' <- a1 b
  case (squares b) ! (0, 0) of
    True -> do
      putStrLn ""
      playGame b' a2 a1
    False -> putStrLn "Game over"

main :: IO ()
main = playGame newBoard humanTurn computerTurn
