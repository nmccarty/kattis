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
{-# LANGUAGE BangPatterns #-}

module Main (
    main
) where

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

getEngineer :: IO Engineer
getEngineer =
  do !line <- getLine
     let [(first,xs1)] = reads line
         [(second,xs2)] = reads xs1
         [(third,xs3)] = reads xs2
     return (first, second, third)

getEngineers :: Int -> IO [Engineer]
getEngineers n = sequence $ replicate n getEngineer

doTestCase :: IO Int
doTestCase =
  do !times <- liftM read getLine :: IO Int
     engineers <- getEngineers times
     return $! length (shortList engineers)

main =
  do !testCases <- liftM read getLine :: IO Int
     values <- sequence $ replicate testCases doTestCase
     mapM print values
