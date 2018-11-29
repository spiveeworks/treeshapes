import Data.Function

data Tree = Leaf | Node Tree Tree deriving Show

count :: Tree -> Integer
count Leaf = 1
count (Node x y) = count x + count y

specDet :: Tree -> Integer
specDet Leaf = 1
specDet (Node x y) = (count x + count y) * specDet x * specDet y

similar :: Tree -> Tree -> Bool
similar = (==) `on` specDet

same :: Tree -> Tree -> Bool
same Leaf Leaf = True
same Leaf _ = False
same _ Leaf = False
same (Node x1 y1) (Node x2 y2) = sameC || sameT
  where
    sameC = same x1 x2 && same y1 y2
    sameT = same x1 y2 && same y1 x2


exhaust :: Integer -> [Tree]
exhaust 1 = [Leaf]
exhaust n = [1..n-1] >>= exhaustOn n

exhaustOn :: Integer -> Integer -> [Tree]
exhaustOn n nl = Node <$> exhaust nl <*> exhaust (n - nl)

findSynonym :: [Tree] -> Maybe (Tree, Tree)
findSynonym ts = foldr retSynonym Nothing tps
  where tps = [(t1, t2) | t1 <- ts, t2 <- ts]
        retSynonym (t1, t2) acc | isSynonym t1 t2 = Just (t1, t2)
                                | otherwise = acc

isSynonym :: Tree -> Tree -> Bool
isSynonym t1 t2 = similar t1 t2 && not (same t1 t2)

displayResult :: Maybe (Tree, Tree) -> String
displayResult Nothing = "No synonyms found"
displayResult (Just (t1,  t2)) = show t1 ++ "\n is similar to \n" ++ show t2

mainPure :: Integer -> String
mainPure = displayResult . findSynonym . exhaust

main :: IO ()
main = do
  inp <- getLine
  let n = read inp
  putStrLn (mainPure n)


resultl =
 Node
  (Node Leaf Leaf)
  (Node Leaf
   (Node
    (Node Leaf Leaf)
    (Node Leaf
     (Node Leaf Leaf))))
-- is similar to
resultr =
 Node
  (Node Leaf
   (Node Leaf Leaf))
  (Node Leaf
   (Node Leaf
    (Node Leaf
     (Node Leaf Leaf))))
