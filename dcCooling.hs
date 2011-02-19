
import Data.Array
import Data.List
import Data.Graph
import Data.Maybe
import Control.Parallel.Strategies

type Node = Int
type Key  = (Int, Int)
type Grid = [(Key, Node)]

sum' = foldl' (+) 0

toGraph :: Grid
        -> (Graph
          , Vertex -> (Node, Key, [Key])
          , Key -> Maybe Vertex)
toGraph g = graphFromEdges 
            $ map (\(k,n) -> (n,k,neighbors k)) 
            $ filter (\(k,n) -> not (n == 1)) g
    where
      neighbors (i, j) = concat [isNeighbor (i+1, j)
                                ,isNeighbor (i-1, j)
                                ,isNeighbor (i, j+1)
                                ,isNeighbor (i, j-1)]
          where
            isNeighbor i = case lookup i g of
                             Nothing -> []
                             Just 1  -> []
                             Just _  -> [i]

removeFrom :: Vertex -> Graph -> ((Vertex -> Vertex), Graph)
removeFrom v g = 
    let f x = if x > v then x-1 else x
        (b1, b2) = bounds g
        as       = assocs g
        notV     = not . (==v)
        ns'      = filter (notV . fst) as
        ns''     = map (\(x,xs) -> (f x, map f $ filter notV xs)) ns'
    in (f, array (b1, b2-1) ns'')

numberOfPaths :: Vertex -> Graph -> Vertex -> Int
numberOfPaths o g i 
    | comps > 1 = 0
    | i == o = if comps == 0 then 1 else 0
    | otherwise = sum' $ parMap rdeepseq (numberOfPaths (change o) g' . change) (g ! i)
    where
      (change,g') = i `removeFrom` g
      comps = length $ components g' 

main :: IO ()
main = do
  l <- getLine
  let nums = map read $ words l :: [Int]
      (maxW, maxH)  = (nums !! 0, nums !! 1)
      ks   = [(x,y) | x <- [0..maxH-1], y <- [0..maxW-1]]
      ls   = drop 2 nums
      (grid, inv) = (zip ks ls, zip ls ks)
      (inK, outK)  = (fromJust $ lookup 2 inv, fromJust $ lookup 3 inv)
      (g, _, f) = toGraph grid
      (i, o)  = (fromJust $ f inK, fromJust $ f outK)
  print $ numberOfPaths o g i
