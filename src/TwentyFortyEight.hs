-----------------------------------------------------------------------------
--
-- Module      :  TwentyFortyEight
-- Copyright   :  Nathan McCarty
-- License     :  AllRightsReserved
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import qualified Data.List
import Control.Arrow ((>>>))

type Board = [[Int]]

readBoard :: String -> Board
readBoard x =
    let rows = lines x
        cols = map words rows
    in map (map read) cols

rotate :: Int -> Board -> Board
rotate 0 b = b
rotate 4 b = b
rotate n b = rotate (n-1) (Data.List.transpose . map reverse $ b)

unrotate :: Int -> Board -> Board
unrotate n = rotate (4 - n)

fallRow :: [Int] -> [Int]
fallRow x = filter (/= 0) x ++ filter (== 0) x

fall :: Board -> Board
fall = map fallRow

combineRow :: [Int] -> [Int]
combineRow (x:xs) = helper x xs
    where helper x [y] =
            if x == y then
                [x+y,0]
            else
                [x,y]
          helper 0 ys = 0 : ys
          helper x (y:ys) =
            if x == y then
                [x+y] ++ helper (head ys) (tail ys) ++ [0]
            else
                x : helper y ys
          helper x [] = [x]

combine :: Board -> Board
combine = map combineRow

swipe :: Int -> Board -> Board
swipe n = rotate n >>> fall >>> combine >>> unrotate n

printboard = Data.List.intercalate "\n" . map (unwords . map show)

main = do
    row1 <- getLine
    row2 <- getLine
    row3 <- getLine
    row4 <- getLine
    direction <- getLine
    let board = readBoard . Data.List.intercalate "\n" $ [row1, row2, row3, row4]
        d =  read direction :: Int
    putStrLn . printboard . swipe d $ board
