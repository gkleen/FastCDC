module Main
  ( main
  ) where

import Prelude
import Data.Digest.GearHash.Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString.Builder as BS.Builder

import System.IO (stdout)

import qualified Data.BitVector.LittleEndian as BitVector
  

main :: IO ()
main = runConduit $ C.stdin .| rollingHash' (BS.Builder.word64BE . BitVector.toUnsignedNumber) .| C.sinkHandleBuilder stdout
