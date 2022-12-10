-- 10.hs - Robert Coffey - 2022-12-10

data Inst = Noop | Addx Int deriving (Show)

chop :: Int -> [a] -> [[a]]
chop len xs
  | length group < len = []
  | otherwise          = group : chop len (drop len xs)
  where group = take len xs

cycleDeltas :: [Inst] -> [Int]
cycleDeltas [] = []
cycleDeltas (Noop:xs) = 0 : cycleDeltas xs
cycleDeltas ((Addx x):xs) = 0 : x : cycleDeltas xs

cycleValues :: Int -> [Int] -> [Int]
cycleValues reg [] = [reg]
cycleValues reg (x:xs) = reg : cycleValues (reg + x) xs

rasterLine :: [Int] -> String
rasterLine vs = rasterLine' 0 vs

rasterLine' :: Int -> [Int] -> String
rasterLine' pix (v:vs) = c : rasterLine' (pix+1) vs
  where c = if abs (pix-v) > 1 then '.' else '#'
rasterLine' 40 _ = []

main :: IO ()
main = do
  content <- readFile "10.dat"
  let insts = -- [Inst]
        map (\line -> case words line of
                        ["noop"]      -> Noop
                        ["addx", num] -> Addx (read num :: Int))
        $ lines content
  let values = cycleValues 1 $ cycleDeltas insts

  -- Part 1
  let sigSum = sum [i * (values !! (i-1)) | i <- [20,60..length values]]
  putStrLn $ "Part 1: " ++ show sigSum

  -- Part 2
  let rows = map rasterLine $ chop 40 values
  putStrLn "Part 2:"
  mapM_ (putStrLn . show) rows
