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
import Data.List (nub, sortBy)
import Data.Function (on)

-- Format of the position is (row, column)
-- Each queen is confined to a row

-- Takes the size of the board, then a list of squares with queens on them
-- Then returns a list of squares that a new queen can be placed on
availableSquares :: Int -> [(Int, Int)] -> [(Int, Int)]
availableSquares size queens =
  let allSquares = [(i,j) | i <- [1..size], j <- [1..size]]
  in filter (not . (\(r,_) -> any (fst >>> (== r)) queens))
     >>> filter (not . (\(_,c) -> any (snd >>> (== c)) queens))
     >>> filter (not . (\p -> any (sameDiagonal p) queens))
     $ allSquares
  where sameDiagonal (r1,c1) (r2,c2) =
          let dr = (fromIntegral $ r1 - r2) :: Rational
              dc = (fromIntegral $ c1 - c2) :: Rational
              slope = dr/dc
          in (abs slope) == 1

addNextQueen :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
addNextQueen size row queens = do
  canidateSquare <- availableSquares size queens
  guard $ fst canidateSquare == row
  let newQueens = canidateSquare:queens
  guard $ (length $ availableSquares size newQueens) > 0
  return newQueens

addQueens :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
addQueens size 0 queens = return queens
addQueens size 1 queens = do
  newQueen <- availableSquares size queens
  let newQueens = newQueen:queens
  return newQueens
addQueens size n queens = do
  newQueens <- addNextQueen size n queens
  addQueens size (n - 1) newQueens

main = do
  let n = 8
      solutions = addQueens n n []
  print $ length solutions
