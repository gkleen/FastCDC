{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Data.Digest.GearHash
  ( GearHashTable
  , mkGearHashTable, gearHashTableHashLength
  , GearHashState
  , hashInit, hashInitWith, defaultGearHashTable, defaultGearHashTableFor
  , hashUpdate
  , BitVector
  , hashFinalize
  ) where

import Prelude
import Data.Array.IArray ((!))
import Data.Word
import Data.Bits (Bits(shiftL))

import Data.Digest.GearHash.Types

import Language.Haskell.TH.Syntax (Lift(liftTyped))

import Data.BitVector.LittleEndian (BitVector)
import qualified Data.BitVector.LittleEndian as BitVector
import Data.Maybe

import Numeric.Natural (Natural)


{-# INLINE hashInitWith #-}
hashInitWith :: GearHashTable -> GearHashState
hashInitWith gearHashTable = GearHashState { gearHashCurrent = BitVector.fromNumber (fromIntegral $ unsafeGearHashTableHashLength gearHashTable) (0 :: Integer), .. }

{-# INLINE hashUpdate #-}
hashUpdate :: Word8 -> GearHashState -> GearHashState
hashUpdate byte !(hState@GearHashState{..}) = hState'
  where
    !hState' = hState { gearHashCurrent = BitVector.fromNumber (BitVector.dimension gearHashCurrent) (BitVector.toUnsignedNumber (gearHashCurrent `shiftL` 1) + BitVector.toUnsignedNumber (unGearHashTable gearHashTable ! byte) :: Natural) }

{-# INLINE hashFinalize #-}
hashFinalize :: GearHashState -> BitVector
hashFinalize = gearHashCurrent


{-# INLINE hashInit #-}
hashInit :: GearHashState
hashInit = hashInitWith defaultGearHashTable
  
{-# NOINLINE defaultGearHashTable #-}
defaultGearHashTable :: GearHashTable
defaultGearHashTable = $$(liftTyped . fromMaybe (error "Could not generate defaultGearHashTable") $ defaultGearHashTableFor 64)
