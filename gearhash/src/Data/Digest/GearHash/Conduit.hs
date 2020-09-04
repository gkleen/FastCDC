{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Digest.GearHash.Conduit
  ( GearHashTable
  , mkGearHashTable
  , rollingHash, rollingHashWith, defaultGearHashTable, defaultGearHashTableFor
  , rollingHash', rollingHashWith'
  , BitVector
  ) where

import Prelude
import Data.Conduit
import Data.Conduit.Combinators (chunksOfE, yieldMany)

import Data.Digest.GearHash

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import Data.Monoid (Endo(..))


rollingHashWith' :: forall m acc.
                    ( Monad m
                    , Monoid acc
                    )
                 => (BitVector -> acc)
                 -> GearHashTable
                 -> ConduitT ByteString acc m ()
rollingHashWith' toAcc tbl = chunksOfE 128 .| go (hashInitWith tbl)
  where
    go hState = do
      chunk' <- await
      case chunk' of
        Nothing -> return ()
        Just chunk -> do
          let (hState', acc) = ByteString.foldl' go' (hState, mempty) chunk
          yield acc
          go hState'

    go' (hState, acc) w = out `seq` (hState', acc <> toAcc out)
      where hState' = hashUpdate w hState
            out = hashFinalize hState'

rollingHash' :: forall m acc.
                ( Monad m
                , Monoid acc
                )
             => (BitVector -> acc)
             -> ConduitT ByteString acc m ()
rollingHash' = flip rollingHashWith' defaultGearHashTable
  

rollingHashWith :: forall m.
                   Monad m
                => GearHashTable
                -> ConduitT ByteString BitVector m ()
rollingHashWith tbl = rollingHashWith' (Endo . (:)) tbl .| awaitForever (yieldMany . flip appEndo [])
  
rollingHash :: forall m.
               Monad m
            => ConduitT ByteString BitVector m ()
rollingHash = rollingHashWith defaultGearHashTable
