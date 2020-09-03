module Main
  ( main
  ) where

import Prelude
import Data.Digest.GearHash.Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString as ByteString
import Data.Bits


main :: IO ()
main = runConduit $ C.stdin .| rollingHash .| C.map pack .| C.stdout
  where
    pack w = ByteString.pack [ fromIntegral $ w `shiftR` s
                             | s <- [56,48..0]
                             ]
