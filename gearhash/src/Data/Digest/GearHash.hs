{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Data.Digest.GearHash
  ( GearHashTable
  , mkGearHashTable
  , GearHashState
  , hashInit, hashInitWith, defaultGearHashTable
  , hashUpdate
  , hashFinalize
  ) where

import Prelude
import Data.Array.IArray ((!))
import Data.Word
import Data.Bits (Bits(shiftL))

import Data.Digest.GearHash.Types

import Crypto.Random
import Language.Haskell.TH.Syntax (Lift(liftTyped))


{-# INLINE hashInitWith #-}
hashInitWith :: GearHashTable -> GearHashState
hashInitWith gearHashTable = GearHashState { gearHashCurrent = 0, .. }

{-# INLINE hashUpdate #-}
hashUpdate :: Word8 -> GearHashState -> GearHashState
hashUpdate byte !(hState@GearHashState{..})
  = hState { gearHashCurrent = (gearHashCurrent `shiftL` 1) + (unGearHashTable gearHashTable ! byte) }

{-# INLINE hashFinalize #-}
hashFinalize :: GearHashState -> Word64
hashFinalize = gearHashCurrent


{-# INLINE hashInit #-}
hashInit :: GearHashState
hashInit = hashInitWith defaultGearHashTable
  
{-# NOINLINE defaultGearHashTable #-}
defaultGearHashTable :: GearHashTable
defaultGearHashTable = $$(liftTyped . unsafeGearHashTableFromByteString . fst . randomBytesGenerate 2048 $ drgNewTest (0, 0, 0, 0, 0))
