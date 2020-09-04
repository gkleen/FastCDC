{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Conduit.Algorithms.FastCDC
  ( fastCDC
  , FastCDCParameters(fastCDCMinBlockSize, fastCDCMaxBlockSize, fastCDCNormalBlockSize, fastCDCMaskSmallBits, fastCDCMaskLargeBits, fastCDCGearHashTable)
  , fastCDCParameters, recommendFastCDCParameters
  ) where

import Prelude
import Data.Conduit
import Data.Digest.GearHash

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import qualified Data.BitVector.LittleEndian as BitVector
import Data.Bits

import Numeric.Natural (Natural)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Control.Monad (guard)

import Data.Ratio ((%))


data FastCDCParameters = FastCDCParameters
  { fastCDCMinBlockSize
  , fastCDCMaxBlockSize
  , fastCDCNormalBlockSize :: !Int
  , fastCDCMaskSmallBits
  , fastCDCMaskLargeBits :: !Int
  , fastCDCWindowSize :: !Int
  , fastCDCGearHashTable :: !GearHashTable
  } deriving (Eq, Ord, Show, Generic, Typeable)

fastCDCParameters :: Int -- ^ Minimum block size
                  -> Int -- ^ Normal block size
                  -> Int -- ^ Maximum block size
                  -> Int -- ^ Mask popcount to generate normal sized chunks
                  -> Int -- ^ Mask popcount to generate small chunks
                  -> GearHashTable
                  -> Maybe FastCDCParameters
fastCDCParameters fastCDCMinBlockSize fastCDCNormalBlockSize fastCDCMaxBlockSize fastCDCMaskSmallBits fastCDCMaskLargeBits fastCDCGearHashTable = do
  fastCDCWindowSize <- gearHashTableHashLength fastCDCGearHashTable
  guard $ fastCDCMinBlockSize >= 0
  guard $ fastCDCMinBlockSize <= fastCDCNormalBlockSize
  guard $ fastCDCMaxBlockSize >= fastCDCNormalBlockSize
  guard $ fastCDCMaxBlockSize > 0
  guard $ fastCDCMaskSmallBits <= fastCDCMaskLargeBits
  guard $ fastCDCMaskLargeBits <= fastCDCWindowSize
  return FastCDCParameters{..}

recommendFastCDCParameters :: Int -- ^ @log2@ of target average block size
                           -> Int -- ^ Rolling hash window size
                           -> Maybe FastCDCParameters
recommendFastCDCParameters targetMaskSize windowSize = fastCDCParameters minBlockSize normalBlockSize maxBlockSize maskSmallBits maskLargeBits =<< defaultGearHashTableFor windowSize
  where
    minBlockSize = floor $ normalBlockSize % 4
    maxBlockSize = normalBlockSize * 8
    normalBlockSize = 2 ^ targetMaskSize
    maskSmallBits = max 0 $ targetMaskSize - 2
    maskLargeBits = targetMaskSize + 2

  
fastCDC :: forall m.
           Monad m
        => FastCDCParameters
        -> ConduitT ByteString ByteString m ()
fastCDC FastCDCParameters{..} = conduitGo ByteString.empty $ hashInitWith fastCDCGearHashTable
  where
    conduitGo :: ByteString -- ^ Chunk accumulator
              -> GearHashState
              -> ConduitT ByteString ByteString m ()
    conduitGo acc hState = do
      nextChunk <- await
      case nextChunk of
        Nothing -> yield acc
        Just nextChunk' -> go acc hState nextChunk' >>= uncurry conduitGo

    go :: forall i.
          ByteString -- ^ Chunk accumulator
       -> GearHashState
       -> ByteString -- ^ Input
       -> ConduitT i ByteString m (ByteString, GearHashState) -- ^ Return new chunk accumulator
    go acc hState inpBS
      | ByteString.null inpBS = return (acc, hState)
      | accLength < fastCDCMinBlockSize
      , (inpBSIgnore, inpBS') <- ByteString.splitAt (fastCDCMinBlockSize - accLength) inpBS
      = go (acc <> inpBSIgnore) hState inpBS'
      where accLength = ByteString.length acc
    go acc hState inpBS
      | breakPos >= inpLength = return (acc <> inpBS, hState3)
      | otherwise = do
          yield $ acc <> inpPrefix
          go ByteString.empty hState3 inpSuffix
      where
        (breakPos, hState3) = go' hState 0
        (inpPrefix, inpSuffix) = ByteString.splitAt breakPos inpBS
        
        go' :: GearHashState -> Int -> (Int, GearHashState)
        go' hState' ix
          | ix >= inpLength
          = (inpLength, hState')
          | ix >= fastCDCMaxBlockSize'
          = (fastCDCMaxBlockSize', hState')
          | otherwise = case compare ix fastCDCNormalBlockSize' of
              LT | not $ testMask hVal maskLarge
                   -> go' hState'' $ succ ix
              _  | not $ testMask hVal maskSmall
                   -> go' hState'' $ succ ix
              _ -> (ix, hState'')
          where
            hState'' = hashUpdate (ByteString.index inpBS ix) hState'
            hVal = hashFinalize hState''

        inpLength = ByteString.length inpBS
        accLength = ByteString.length acc
        fastCDCMaxBlockSize' = fastCDCMaxBlockSize - accLength
        fastCDCNormalBlockSize' = fastCDCNormalBlockSize - accLength
    
    maskSmall, maskLarge :: BitVector
    !maskSmall = mkMask fastCDCMaskSmallBits
    !maskLarge = mkMask fastCDCMaskLargeBits

    mkMask :: Int -> BitVector
    mkMask maskSize = BitVector.fromNumber (fromIntegral fastCDCWindowSize) ((pred $ 1 `shift` maskSize) `shiftL` (fastCDCWindowSize - maskSize) :: Natural)

    testMask :: BitVector -> BitVector -> Bool
    testMask v mask = BitVector.isZeroVector $ v .&. mask
