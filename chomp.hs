-- Copyright © 2013 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Chomp player in Haskell

import Data.Array
import Text.Printf

data Move = Move Int Int

newtype Board = Board (Array (Int, Int) Bool)

rows :: Int
rows = 3
cols :: Int
cols = 4

newBoard :: Board
newBoard = Board $ 
             listArray ((0, 0), (rows - 1, cols - 1)) $ 
             repeat True

instance Show Board where
  show (Board b) = unlines $ 
           [ concat $
             [ if b ! (i, j) then "o" else "." |
               j <- [0 .. cols - 1] ] |
             i <- [0 .. rows - 1] ]

instance Show Move where
  show (Move r c) = show (r, c)

getMove :: IO Move
getMove = do
  moveStr <- getLine
  let (r, c) = read moveStr
  return $ Move r c

makeMove :: Board -> Move -> Board
makeMove (Board b) (Move r0 c0) =
  Board $
  b // [ ((r, c), False) |
         r <- [r0 .. rows - 1],
         c <- [c0 .. cols - 1] ]

negamax :: Board -> Either (Maybe Move) Move
negamax b@(Board b0) =
  let (0, 0) : squares = map fst $ filter snd $ assocs b0 in
  case findJust $ map tryNegamax squares of
     Just m -> Right m 
     Nothing -> 
       case squares of
         [] -> Left Nothing
         _ -> Left $ Just $ toMove $ last squares
  where
    findJust (Just x : _) = Just x
    findJust (_ : xs) = findJust xs
    findJust [] = Nothing
    tryNegamax (r, c) =
      case negamax (makeMove b (Move r c)) of
        Right m -> Just m
        Left _ -> Nothing
    toMove (r, c) = Move r c

type Action = Board -> IO Board

humanTurn :: Action
humanTurn b = do
  m <- getMove
  return $ makeMove b m

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
  b'@(Board b'0) <- a1 b
  case b'0 ! (0, 0) of
    True -> do
      putStrLn ""
      playGame b' a2 a1
    False -> putStrLn "Game over"

main :: IO ()
main = playGame newBoard humanTurn computerTurn
