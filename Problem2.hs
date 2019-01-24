-- IPv4 Permutations

module Problem2 where

import Data.List (intercalate)
import Data.Word (Word8)

data IPv4 = IPv4 Word8 Word8 Word8 Word8

instance Show IPv4 where
  show (IPv4 o1 o2 o3 o4) = intercalate "." $ map show [o1, o2, o3, o4]

possibleAddresses :: String -> [IPv4]
possibleAddresses = undefined
