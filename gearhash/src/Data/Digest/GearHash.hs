{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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


hashInitWith :: GearHashTable -> GearHashState
hashInitWith gearHashTable = GearHashState { gearHashCurrent = 0, .. }

hashUpdate :: Word8 -> GearHashState -> GearHashState
hashUpdate byte GearHashState{..} = GearHashState
  { gearHashCurrent = (gearHashCurrent `shiftL` 1) + gear byte
  , ..
  }
  where gear = (unGearHashTable gearHashTable !)

hashFinalize :: GearHashState -> Word64
hashFinalize = gearHashCurrent


hashInit :: GearHashState
hashInit = hashInitWith defaultGearHashTable
  
{-# NOINLINE defaultGearHashTable #-}
defaultGearHashTable :: GearHashTable
defaultGearHashTable = $$(liftTyped . unsafeGearHashTableFromByteString . fst . randomBytesGenerate 2048 $ drgNewTest (0, 0, 0, 0, 0))
