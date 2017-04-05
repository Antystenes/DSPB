module Main where

import Prelude hiding (Either(..))
import Lib
import Control.Monad
import Control.Concurrent
import Control.Comonad
import System.Console.ANSI

main :: IO ()
main =
  --molDir <$> (diffusion . moveGrid Up. moveGrid Right) sampleNGrid >>= print
  --simulationN
  simulationNIO
  --simulationGOF
  --foldM_ (\ac f -> let n = ac =>> f in clearScreen >> print n >> threadDelay 1000000 >> return n) sampleGrid $ (extract : repeat conwayUpdate)
