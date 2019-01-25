-- IPv4 Permutations

module Problem2 where

import Control.Monad ((>=>), guard)
import Data.Char (isDigit, digitToInt)
import Data.List (intercalate, inits, tails, foldl', nub)
import Data.Word (Word8)

type Octet = Word8

data IPv4 = IPv4 Octet Octet Octet Octet
  deriving Eq

instance Show IPv4 where
  show (IPv4 o1 o2 o3 o4) = intercalate "." $ map show [o1, o2, o3, o4]

parseDigit :: Char -> Maybe Int
parseDigit c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

parseDigits :: String -> Maybe [Int]
parseDigits = traverse parseDigit

-- All possible contiguous slices of minimum size s
-- Takes advantage of monadic Kleisli composition of lists
slices :: Int -> [a] -> [[a]]
slices s = tails >=> (drop s . inits)

-- Given a list, return all possible partitions of the list into k nonempty
-- substrings
--
-- Strategy:
-- * Split at each point from 1 .. length - k + 1, enough to leave n - 1
--   elements
-- * Generate all (kpartitions (k - 1)) of the second list
-- * Append the first list to each of the partitioned lists
-- * Concatenate all results
kpartitions :: Int -> [a] -> [[[a]]]
kpartitions _ []  = []
kpartitions 0 _   = []
kpartitions 1 xs  = [[xs]]
kpartitions k xs  = do
  let len = length xs
  splitIx <- [1..(len - k + 1)]
  let (left, right) = splitAt splitIx xs
  rightParts <- kpartitions (k - 1) right
  return $ left : rightParts

-- Convert base-10 list of digits to a number
-- If this was restricted to Word8s, it would wrap after 255 and validate any
-- three-digit value
digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\acc n -> 10 * acc + n) 0

-- Join a group of digits into an octet
joinOctet :: [Int] -> Octet
joinOctet = fromIntegral . digitsToInt

-- Determine whether a list of digits is a valid octet group
-- Assumes that the elements of xs are valid digits, i.e. 0 ≤ x ≤ 9 ∀ x ∈ xs
validOctetGroup :: [Int] -> Bool
validOctetGroup []        = False
validOctetGroup [_]       = True
validOctetGroup [x, _]    = x /= 0
validOctetGroup [x, y, z] = x /= 0 && digitsToInt [x, y, z] <= 255
validOctetGroup _         = False

ipv4Partitions :: [Int] -> [IPv4]
ipv4Partitions s = do
  slice  <- slices 4 s
  octets <- kpartitions 4 slice
  guard $ all validOctetGroup octets
  let [o1, o2, o3, o4] = map joinOctet octets
  return $ IPv4 o1 o2 o3 o4

-- There is not a trivial way to make sure that the output list of IPv4
-- addresses will be unique, so a nub is unfortunately necessary to ensure
-- uniqueness of results
possibleAddresses :: String -> [IPv4]
possibleAddresses s = maybe [] (nub . ipv4Partitions) $ parseDigits s
