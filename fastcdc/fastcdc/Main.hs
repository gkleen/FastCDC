module Main
  ( main
  ) where

import Prelude
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Algorithms.FastCDC

import qualified Data.ByteString as ByteString
import Data.Maybe


main :: IO ()
main = runConduit $ C.stdin .| fastCDC params .| C.map ByteString.length .| C.print
  where params = fromMaybe (error "Could not recommend FastCDC Params")
          $ recommendFastCDCParameters 13 64
