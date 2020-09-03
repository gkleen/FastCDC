{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Digest.GearHash.Types where

import Prelude
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray (array, (!))
import Data.Word
import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import Data.Maybe


newtype GearHashTable
  = GearHashTable { unGearHashTable :: UArray Word8 Word64 }
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

instance Lift GearHashTable where
  liftTyped tbl = [||unsafeGearHashTableFromByteString $$(liftTyped $ gearHashTableToByteString tbl)||]

data GearHashState = GearHashState
  { gearHashTable   :: GearHashTable
  , gearHashCurrent :: {-# UNPACK #-} !Word64
  } deriving (Eq, Ord, Show, Generic, Typeable)


mkGearHashTable :: (Word8 -> Word64) -> GearHashTable
mkGearHashTable f = GearHashTable $! array (0, 255) [(w, f w) | w <- [0..255]]

unsafeGearHashTableFromByteString :: ByteString -> GearHashTable
unsafeGearHashTableFromByteString = fromMaybe (error "Could not convert GearHashTable from ByteString") . gearHashTableFromByteString

gearHashTableFromByteString :: ByteString -> Maybe GearHashTable
gearHashTableFromByteString = fmap mkGearHashTable . go 0 (\ix -> error $ "No GearHashTable value available for " ++ show ix)
  where
    go :: Int -> (Word8 -> Word64) -> ByteString -> Maybe (Word8 -> Word64)
    go ix acc bs | ix > 255
                 , ByteString.null bs
                 = Just acc
                 | ix > 255
                 = Nothing
    go ix acc bs = case ByteString.splitAt 8 bs of
      (ws, bs')
        | ByteString.length ws == 8
          -> let res = foldr (\(s, w) wacc -> wacc .|. fromIntegral w `shiftL` s) 0 . zip [56,48..] $ ByteString.unpack ws
                 acc' inp
                   | inp == fromIntegral ix = res
                   | otherwise = acc inp
              in go (succ ix) acc' bs'
      _other -> Nothing

gearHashTableToByteString :: GearHashTable -> ByteString
gearHashTableToByteString = fst . ByteString.unfoldrN 2048 build . (0,[],)
  where
    build :: (Int, [Word8], GearHashTable) -> Maybe (Word8, (Int, [Word8], GearHashTable))
    build (ix, [], _) | ix > 255 = Nothing
    build (ix, w:ws, tbl) = Just (w, (ix, ws, tbl))
    build (ix, [], tbl) = Just (w1, (succ ix, ws, tbl))
      where
        w = unGearHashTable tbl ! fromIntegral ix
        (w1:ws) = [ fromIntegral $ w `shiftR` s
                  | s <- [56,48..0] 
                  ]
