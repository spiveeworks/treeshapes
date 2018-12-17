module Tree
( Tree(Leaf, Branch)
, subtrees
, count
, counts
, same
, exhaust
) where

data Tree = Leaf | Branch Tree Tree deriving Show

subtrees :: Tree -> [Tree]
subtrees Leaf = [Leaf]
subtrees t@(Branch l r) = t : subtrees l ++ subtrees r

count :: Tree -> Integer
count Leaf = 1
count (Branch x y) = count x + count y

counts :: Tree -> [Integer]
counts = map count . subtrees

same :: Tree -> Tree -> Bool
same Leaf Leaf = True
same Leaf _ = False
same _ Leaf = False
same (Branch x1 y1) (Branch x2 y2) = sameC || sameT
  where
    sameC = same x1 x2 && same y1 y2
    sameT = same x1 y2 && same y1 x2


exhaust :: Integer -> [Tree]
exhaust 1 = [Leaf]
exhaust n = [1..n-1] >>= exhaustOn n

exhaustOn :: Integer -> Integer -> [Tree]
exhaustOn n nl = Branch <$> exhaust nl <*> exhaust (n - nl)

