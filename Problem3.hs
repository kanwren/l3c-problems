-- Largest Contiguous Sum

-- The simplest way to solve the largest contiguous sum problem is using
-- Kadane's algorithm, which solves it in O(n) time and O(1) space

module Problem3 where

import Data.List (foldl')

-- Need a strict fold (foldl') to avoid stack overflows from lazy foldl
-- This is more efficient over Integers than lazy evaluation
maxContSum :: [Integer] -> Integer
maxContSum = snd . foldl' kadane (0, 0)
  where kadane (curMax, totalMax) x =
          let curMax' = max x (curMax + x)
              totalMax' = max totalMax curMax'
           in (curMax', totalMax')
