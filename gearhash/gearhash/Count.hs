module Main
  ( main
  ) where

import Prelude

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString.Builder as BS.Builder

import System.IO (stdout)


main :: IO ()
main = runConduit $ C.enumFromTo minBound maxBound .| C.map BS.Builder.word32LE .| C.sinkHandleBuilder stdout
