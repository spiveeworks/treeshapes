import Data.Function
import Data.List

import Tree

similar :: Tree -> Tree -> Bool
similar = (==) `on` (sort . diag)

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

ch = Branch Leaf Leaf
q1 = Branch ch ch
q2 = Branch Leaf (Branch Leaf ch)

result1 = Branch (Branch q1 Leaf) q2
-- is similar to
result2 = Branch q1 (Branch Leaf q2)

