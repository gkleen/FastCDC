module Main
  ( main
  ) where

import Prelude
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as BS.Builder

import Data.Digest.GearHash
import System.IO (stdin, stdout)
import Control.Monad (unless)
  
import qualified Data.BitVector.LittleEndian as BitVector
  

main :: IO ()
main = go hashInit
  where
    go hState = do
      inpBS <- ByteString.hGetSome stdin 128
      unless (ByteString.null inpBS) $ do
        let (hState', acc) = ByteString.foldl' go' (hState, mempty) inpBS
        BS.Builder.hPutBuilder stdout acc
        go hState'

    go' (hState, acc) w = out `seq` (hState', acc <> BS.Builder.word64BE out)
      where hState' = hashUpdate w hState
            out = BitVector.toUnsignedNumber $ hashFinalize hState'
