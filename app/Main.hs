module Main where

import Lib
import Control.Monad
import Control.Concurrent
import Control.Comonad
import System.Console.ANSI

main :: IO ()
main = simulationN
  --foldM_ (\ac f -> let n = ac =>> f in clearScreen >> print n >> threadDelay 1000000 >> return n) sampleGrid $ (extract : repeat conwayUpdate)
