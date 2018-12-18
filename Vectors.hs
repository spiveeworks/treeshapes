module Vectors
( vectors
, vectorsMat
) where
import Tree
import Data.List

vec :: Integer -> a -> [a]
vec size val = take (fromInteger size) (repeat val)

vectors :: Tree -> [[Integer]]
vectors t = vec (count t) 1 : symVectors t

symVectors :: Tree -> [[Integer]]
symVectors Leaf = []
symVectors (Branch lt rt) = result where
  l = count lt
  r = count rt
  ls = map (++vec r 0) $ symVectors lt
  rs = map (vec l 0++) $ symVectors rt
  result = root l r : ls ++ rs

root :: Integer -> Integer -> [Integer]
root l r = vec l r ++ vec r (-l)

vectorsMat :: Tree -> [[Integer]]
vectorsMat = transpose . vectors
