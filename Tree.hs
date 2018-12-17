module Tree
( Tree(Leaf, Branch)
, count
, diag
, same
, exhaust
) where

data Tree = Leaf | Branch Tree Tree deriving Show

count :: Tree -> Integer
count Leaf = 1
count (Branch x y) = count x + count y

specDet :: Tree -> Integer
specDet Leaf = 1
specDet (Branch x y) = (count x + count y) * specDet x * specDet y

diag :: Tree -> [Integer]
diag Leaf = []
diag t@(Branch x y) = count t : diag x ++ diag y

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

