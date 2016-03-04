-----------------------------------------------------------------------------
--
-- Module      :  Holey N-Queens (Batman)
-- Copyright   :  Nathan McCarty
-- License     :  AllRightsReserved
--
-- |
--
-----------------------------------------------------------------------------

module Main (
  main
  ) where

import Control.Arrow ((>>>))
import Control.Monad (guard, mapM)
import Data.List (nub, sortBy,(\\))
import Data.Function (on)
import Data.List.Split (chop)
import Debug.Trace

-- Format of the position is (row, column)
-- Each queen is confined to a row

-- Takes the size of the board, then a list of squares with queens on them
-- Then returns a list of squares that a new queen can be placed on
availableSquares :: [(Int,Int)] -> [(Int, Int)] -> [(Int, Int)]
availableSquares board queens =
  filter (not . (\(r,_) -> any (fst >>> (== r)) queens))
  >>> filter (not . (\(_,c) -> any (snd >>> (== c)) queens))
  >>> filter (not . (\p -> any (sameDiagonal p) queens))
  $ board
  where sameDiagonal (r1,c1) (r2,c2) =
          let dr = (fromIntegral $ r1 - r2) :: Rational
              dc = (fromIntegral $ c1 - c2) :: Rational
              slope = dr/dc
          in (abs slope) == 1

addNextQueen :: [(Int,Int)] -> Int -> [(Int, Int)] -> [[(Int, Int)]]
addNextQueen board row queens = do
  canidateSquare <- availableSquares board queens
  guard $ fst canidateSquare == row
  let newQueens = canidateSquare:queens
  guard $ (length $ availableSquares board newQueens) > 0
  return newQueens

addQueens :: [(Int,Int)] -> Int -> [(Int, Int)] -> [[(Int, Int)]]
addQueens board 0 queens = return queens
addQueens board 1 queens = do
  newQueen <- availableSquares board queens
  let newQueens = newQueen:queens
  return newQueens
addQueens board n queens = do
  newQueens <- addNextQueen board n queens
  addQueens board (n - 1) newQueens

main = do
  input <- getContents
  let boards = filter (\(_,x) -> x > 0) . chop consumeSection . lines $ input
      solveBoard (board, size) = length $ addQueens board size []
      solutions = map solveBoard boards
  mapM print solutions
      

consumeSection :: [String] -> (([(Int,Int)],Int),[String])
consumeSection strings =
  let head:tail = strings
      [size, holes] = map read $ words head :: [Int]
      (points, rest) = splitAt holes tail
      holePoints = map (\[x,y] -> (x+1,y+1)) . map (map read) . map words $ points :: [(Int, Int)]
      entireBoard = [(x,y) | x <- [1..size], y <- [1..size]]
      board = entireBoard\\holePoints
  in ((board, size), rest)


      
