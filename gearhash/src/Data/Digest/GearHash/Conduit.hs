{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Digest.GearHash.Conduit
  ( GearHashTable
  , mkGearHashTable
  , rollingHash, rollingHashWith, defaultGearHashTable
  ) where

import Prelude
import Data.Conduit
import Data.Conduit.Lift

import Data.MonoTraversable
import Data.Sequences (IsSequence)

import Data.Word

import Data.Digest.GearHash

import Control.Monad.State.Class


rollingHashWith :: forall mono m.
                   ( Monad m
                   , IsSequence mono, Element mono ~ Word8
                   )
                => GearHashTable
                -> ConduitT mono Word64 m ()
rollingHashWith tbl = evalStateC (hashInitWith tbl) . awaitForever $ \chunk ->
  oforM_ chunk $ \w -> do
    modify' $ hashUpdate w
    yieldM $ gets hashFinalize
  
rollingHash :: forall mono m.
               ( Monad m
               , IsSequence mono, Element mono ~ Word8
               )
            => ConduitT mono Word64 m ()
rollingHash = rollingHashWith defaultGearHashTable
