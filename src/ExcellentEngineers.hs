-----------------------------------------------------------------------------
--
-- Module      :  ExcellentEngineers
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

{-# LANGUAGE BangPatterns #-}

import Control.Monad (liftM)
import Control.Arrow ((>>>))
import Data.List (sortBy, nub)
import Data.Function (on)
import System.IO (getContents)
import Control.Concurrent

type Engineer = (Int, Int, Int)

communication (x,_,_) = x
programming (_,x,_) = x
algorithims (_,_,x) = x

betterAt :: (Engineer -> Int) -> Engineer -> Engineer -> Bool
betterAt f engineer other =
    let x = f engineer
        y = f other
    in x < y

betterAtAll :: Engineer -> Engineer -> Bool
betterAtAll engineer other =
    betterAt communication engineer other
    && betterAt programming engineer other
    && betterAt algorithims engineer other

seperateAndSort :: [Engineer] -> ([Engineer],[Engineer],[Engineer])
seperateAndSort list = (sortBy (compare `on` communication) list
                       , sortBy (compare `on` programming) list
                       , sortBy (compare `on` algorithims) list)

shortList :: [Engineer] -> [Engineer]
shortList list =
    filter betterThanOthersInAll com
    where (com, prog, algo) = seperateAndSort list
          betterThanMeIn :: (Engineer -> Int) -> Engineer -> [Engineer] -> [Engineer]
          betterThanMeIn f engineer list =
            let canidates = takeWhile (\x -> f x < f engineer) list
                betterThanMe = filter (`betterAtAll` engineer) canidates
            in betterThanMe
          betterThanOthersInAll :: Engineer -> Bool
          betterThanOthersInAll engineer =
            let comCanidates = betterThanMeIn communication engineer com
                progCanidates = betterThanMeIn programming engineer prog
                algoCanidates = betterThanMeIn algorithims engineer algo
            in null . nub $ comCanidates ++ progCanidates ++ algoCanidates

main = do
    input <- liftM lines getContents
    let testCases = (read . head $ input) :: Int
    doTestCases testCases $ tail input
    where doTestCases 0 x = return ()
          doTestCases n input =
            do let numLines = read . head $ input
                   lines = take numLines (drop 1 input)
                   rest = drop (numLines + 1) input
                   strings = map words $! lines
                   engineers = map parseEngineer strings
               print . length $ shortList engineers
               doTestCases (n-1) $! rest
          parseEngineer :: [String] -> Engineer
          parseEngineer xs =
            let [a,b,c] = map read $! xs
            in (a,b,c)
