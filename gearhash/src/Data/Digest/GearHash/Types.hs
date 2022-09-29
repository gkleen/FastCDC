{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Digest.GearHash.Types where

import Prelude
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Array (Array)
import Data.Array.IArray (array, (!))
import Data.Word
import Data.Bits
import Data.BitVector.LittleEndian (BitVector)
import Data.Ratio ((%))
import qualified Data.BitVector.LittleEndian as BitVector

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax

import Data.Maybe
import Data.Monoid (Endo(..))

import Crypto.Random


newtype GearHashTable
  = GearHashTable { unGearHashTable :: Array Word8 BitVector }
  deriving (Eq, Ord, Generic, Typeable)

instance Show GearHashTable where
  showsPrec d (GearHashTable _) = showParen (d > 10) $ showString "GearHashTable _"

instance Lift GearHashTable where
  liftTyped tbl = [||unsafeGearHashTableFromByteString $$(liftTyped $ unsafeGearHashTableHashLength tbl) $$(liftTyped $ gearHashTableToByteString tbl)||]

data GearHashState = GearHashState
  { gearHashTable   :: GearHashTable
  , gearHashCurrent :: {-# UNPACK #-} !BitVector
  } deriving (Eq, Ord, Show, Generic, Typeable)


mkGearHashTable :: (Word8 -> BitVector) -> GearHashTable
mkGearHashTable f = GearHashTable $! array (minBound, maxBound) [(w, f w) | w <- [minBound..maxBound]]


{-# NOINLINE defaultGearHashTableFor #-}
defaultGearHashTableFor :: Int -> Maybe GearHashTable
defaultGearHashTableFor l = gearHashTableFromByteString l . fst . randomBytesGenerate (32 * l') $ drgNewTest (fromIntegral l, 0, 0, 0, 0)
  where l' = 8 * ceiling (l % 8)


gearHashTableHashLength :: GearHashTable -> Maybe Int
gearHashTableHashLength (GearHashTable arr) = flip appEndo Nothing $ foldMap (Endo . max . Just . finiteBitSize) arr

unsafeGearHashTableHashLength :: GearHashTable -> Int
unsafeGearHashTableHashLength = fromMaybe (error "Could not determine hash length of GearHashTable") . gearHashTableHashLength


unsafeGearHashTableFromByteString :: Int -> ByteString -> GearHashTable
unsafeGearHashTableFromByteString l = fromMaybe (error "Could not convert GearHashTable from ByteString") . gearHashTableFromByteString l

gearHashTableFromByteString :: Int -> ByteString -> Maybe GearHashTable
gearHashTableFromByteString l _ | l <= 0 = Nothing
gearHashTableFromByteString l inpBS = fmap mkGearHashTable $ go 0 (\ix -> error $ "No GearHashTable value available for " ++ show ix) inpBS
  where
    go :: Int -> (Word8 -> BitVector) -> ByteString -> Maybe (Word8 -> BitVector)
    go ix acc bs | ix > 255
                 , ByteString.null bs
                 = Just acc
                 | ix > 255
                 = Nothing
    go ix acc bs = case ByteString.splitAt (ceiling $ l % 8) bs of
      (ws, bs')
        | ByteString.length ws == ceiling (l % 8)
          -> let res = foldr (\(s, w) wacc -> wacc .|. BitVector.fromNumber (fromIntegral l) w `shiftL` s) (BitVector.fromNumber (fromIntegral l) (0 :: Integer)) . zip [l' - 8,l' - 16..] $ ByteString.unpack ws
                 acc' inp
                   | inp == fromIntegral ix = res
                   | otherwise = acc inp
              in go (succ ix) acc' bs'
      _other -> Nothing

    l' = 8 * ceiling (l % 8)

gearHashTableToByteString :: GearHashTable -> ByteString
gearHashTableToByteString inpTbl = fst $ ByteString.unfoldrN (32 * l') build (0, [], inpTbl)
  where
    build :: (Int, [Word8], GearHashTable) -> Maybe (Word8, (Int, [Word8], GearHashTable))
    build (ix, [], _) | ix > 255 = Nothing
    build (ix, w:ws, tbl) = Just (w, (ix, ws, tbl))
    build (ix, [], tbl) = Just (w1, (succ ix, ws, tbl))
      where
        w = unGearHashTable tbl ! fromIntegral ix
        (w1:ws) = [ BitVector.toUnsignedNumber $ w `shiftR` s
                  | s <- [l' - 8,l' - 16..0]
                  ]

    l' = 8 * ceiling (l % 8)
    l = unsafeGearHashTableHashLength inpTbl
