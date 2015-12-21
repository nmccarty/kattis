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

import Control.Monad (liftM, replicateM)
import Control.Arrow ((>>>))
import Data.List (sortBy, nub)
import Data.Function (on)
import System.IO (getContents)

data Engineer = Engineer {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

communication (Engineer x _ _) = x
programming (Engineer _ x _) = x
algorithims (Engineer _ _ x) = x

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

shortList :: [Engineer] -> [Engineer]
shortList list =
    filter betterThanOthersInAll com
    where com = sortBy (compare `on` communication) list
          betterThanMeIn :: (Engineer -> Int) -> Engineer -> [Engineer] -> [Engineer]
          betterThanMeIn f engineer list =
            let canidates = takeWhile (\x -> f x < f engineer) list
                betterThanMe = filter (`betterAtAll` engineer) canidates
            in betterThanMe
          betterThanOthersInAll :: Engineer -> Bool
          betterThanOthersInAll engineer =
            let canidates = betterThanMeIn communication engineer com
            in null canidates

getEngineer :: IO Engineer
getEngineer =
  do !line <- getLine
     let [(first,xs1)] = reads line
         [(second,xs2)] = reads xs1
         [(third,xs3)] = reads xs2
     return $! Engineer first second third

getEngineers :: Int -> IO [Engineer]
getEngineers n = replicateM n getEngineer

doTestCase :: IO Int
doTestCase =
  do !times <- liftM read getLine :: IO Int
     engineers <- getEngineers times
     return $! length (shortList engineers)

main =
  do !testCases <- liftM read getLine :: IO Int
     values <- replicateM testCases doTestCase
     mapM print values
