-- 9-1.hs - Robert Coffey - 2022-12-09

import Data.List

type Step = Char
type Pos = (Int, Int)

step :: Step -> Pos -> Pos -> (Pos, Pos)
step s (hx,hy) (tx,ty) =
  let (dx,dy) = case s of
                  'U' -> (0,-1)
                  'D' -> (0,1)
                  'L' -> (-1,0)
                  'R' -> (1,0)
      (hx',hy') = (hx+dx, hy+dy)
      dist = max (abs (hx'-tx)) (abs (hy'-ty))
  in ((hx',hy'), if dist > 1 then (hx,hy) else (tx,ty))

-- Execute a list of steps given the starting positions of head and tail, and
-- return a list containing each position tail reached.
exec :: [Step] -> Pos -> Pos -> [Pos]
exec [] h t = [t]
exec (s:ss) h t = t : exec ss h' t'
  where (h', t') = step s h t

main :: IO ()
main = do
  content <- readFile "9.dat"
  let steps = -- String
        concat
        $ map (\line -> take (read $ words line !! 1 :: Int)
                        $ repeat $ head line)
        $ lines content
  let tps = exec steps (0,0) (0,0)
  putStrLn $ show . length . nub $ tps
