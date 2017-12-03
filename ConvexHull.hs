import Text.Printf
import Data.List (nub)

type Point = (Int, Int)
type Points = [Point]

lefter :: Point -> Point -> Point
lefter (x1, y1) (x2, y2)
    | x1 <= x2 = (x1, y1)
    | otherwise = (x2, y2)

righter :: Point -> Point -> Point
righter (x1, y1) (x2, y2)
    | x1 >= x2 = (x1, y1)
    | otherwise = (x2, y2)

leftMost :: Points -> Point
leftMost (p:ps) = foldr lefter p ps

rightMost :: Points -> Point
rightMost (p:ps) = foldr righter p ps

distance :: Point -> Point -> Point -> Double
distance (ax, ay) (bx, by) (px, py) = 
    abs ((by' - ay') * px' - (bx' - ax') * py' + bx' * ay' - by' * ax') / (sqrt (by' - ay') * (by' - ay') + (bx' - ax') * (bx' - ax'))
    where ax' = fromIntegral ax
          ay' = fromIntegral ay
          bx' = fromIntegral bx
          by' = fromIntegral by
          px' = fromIntegral px
          py' = fromIntegral py

distanceBetweenPoints :: Point -> Point -> Double
distanceBetweenPoints (ax, ay) (bx, by) = abs ( sqrt ((bx' - ax') * (bx' - ax') + (by' - ay') * (by' - ay')) )
    where ax' = fromIntegral ax
          ay' = fromIntegral ay
          bx' = fromIntegral bx
          by' = fromIntegral by

farther :: Point -> Point -> Point -> Point -> Point
farther a b p1 p2
    | distance a b p1 >= distance a b p2 = p1
    | otherwise = p2

farthest :: Point -> Point -> Points -> Point
farthest a b (p:ps) = foldr (farther a b) p ps

rightSide :: Point -> Point -> Point -> Bool
rightSide (ax, ay) (bx, by) (px, py) = (bx - ax)*(py - ay) - (by - ay)*(px - ax) < 0

quickHull :: Points -> Points
quickHull s = nub $ [a] ++ findHull s1 a b ++ [b] ++ findHull s2 b a
    where
        a = leftMost s
        b = rightMost s
        s1 = filter (rightSide a b) s'
        s2 = filter (rightSide b a) s'
        s' = filter (\x -> (x /= a) && (x /= b)) s

findHull :: Points -> Point -> Point -> Points
findHull [] _ _ = []
findHull sk p q = findHull s1 p c ++ [c] ++ findHull s2 c q
    where c = farthest p q sk
          s1 = filter (rightSide p c) sk'
          s2 = filter (rightSide c q) sk'
          sk' = filter (\x -> (x /= p) && (x /= q) && (x /= c)) sk

perimeter :: [(Int, Int)] -> Double
perimeter [] = 0
perimeter (p:ps) = sum $ zipWith distanceBetweenPoints (p:ps) (ps ++ [p])

solve :: [(Int, Int)] -> Double
solve points 
    | hull == quickHull hull = perimeter hull   -- it works. I don't know how nor why :P
    | otherwise = solve hull
    where hull = quickHull points

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = take n $ (map ((\ [x, y] -> (x, y)) . map (read :: String -> Int) . words) . lines) content
    ans = solve points
  printf "%.1f\n" ans
