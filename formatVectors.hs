import Tree
import Vectors

import Data.List

formatMat :: [[Integer]] -> String
formatMat xxs = "[" ++ (intercalate "; " $ map formatRow $ xxs) ++ "]"
  where formatRow xs = intercalate " " $ map show $ xs

formatStatement :: (Integer, [[Integer]]) -> String
formatStatement (i, xxs) =
  "  mats(:,:," ++ show i ++ ") = " ++ formatMat xxs ++ ";\n"

formatProgram :: String -> [(Integer, [[Integer]])] -> String
formatProgram name xs = "function mats = " ++ name ++ "()\n" ++ mats ++ "end\n"
  where mats = xs >>= formatStatement

generateProgram :: String -> Integer -> String
generateProgram name
 = formatProgram name
 . (zip $ iterate (+1) $ 1)
 . map vectorsMat
 . exhaust

writeProgram :: Integer -> IO ()
writeProgram n = writeFile fname program where
  fname = name ++ ".m"
  name = "LoadMats" ++ show n
  program = generateProgram name n

main :: IO ()
main = readLn >>= writeProgram
