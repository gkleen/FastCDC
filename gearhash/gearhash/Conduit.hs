module Main
  ( main
  ) where

import Prelude
import Data.Digest.GearHash.Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString.Builder as BS.Builder

import System.IO (stdout)
  

main :: IO ()
main = runConduit $ C.stdin .| rollingHash' BS.Builder.word64BE .| C.sinkHandleBuilder stdout
