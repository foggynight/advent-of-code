import Data.List

type Step = Char
type Knot = (Int, Int)
type Rope = [Knot]

-- To step: walk along each knot in ks and move it based on the step s for the
-- head, or the move of the previous knot for the rest.

step :: Step -> Rope -> Rope
step s ((x,y):ks) = p : rest p ks
  where (dx,dy) = case s of
                    'U' -> (0,-1)
                    'D' -> (0,1)
                    'L' -> (-1,0)
                    'R' -> (1,0)
        p = (x+dx, y+dy)

rest :: Knot -> Rope -> Rope
rest _ [] =  []
rest (px,py) ((x,y):ks) = np : rest np ks
  where (dx,dy) = (px-x, py-y)
        (mx,my) = if (abs dx) > 1 || (abs dy) > 1
                  then (signum dx, signum dy) else (0,0)
        np = (x+mx, y+my)

-- Execute a list of steps given the starting positions of a list of knots,
-- return a list containing each position tail reached.
exec :: [Step] -> Rope -> Rope
exec [] ks = [last ks]
exec (s:ss) ks = last ks : exec ss ks'
  where ks' = step s ks

main :: IO ()
main = do
  content <- readFile "9.dat"
  let steps = -- String
        concat
        $ map (\line -> take (read $ words line !! 1 :: Int)
                        $ repeat $ head line)
        $ lines content
  let tps = exec steps $ take 10 $ repeat (0,0)
  putStrLn $ show . length . nub $ tps
