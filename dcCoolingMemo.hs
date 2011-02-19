-- module DcCooling where 

import qualified Data.Map as Map
import qualified Data.MemoCombinators as Memo
import Data.Maybe

type Grid = Map.Map (Int, Int) Int

pairsMemo :: Memo.Memo (Int, Int)
pairsMemo = Memo.pair Memo.integral Memo.integral

elementMemo :: Memo.Memo ((Int, Int), Int)
elementMemo = Memo.pair pairsMemo Memo.integral

listMemo :: Memo.Memo [((Int, Int), Int)]
listMemo = Memo.list elementMemo

gridMemo :: Memo.Memo Grid
gridMemo = Memo.wrap Map.fromList Map.toList listMemo

toAcGrids :: (Int, Int) -> Grid -> [Grid] 
toAcGrids (maxW, maxH) fstG = memoizedBuildGrids intake fstG
    where 
      buildGrids (i, j) g
          | i == maxH || i < 0 || j == maxW || j < 0 || el == 1 = []
          | el == 3 = [g]
          | otherwise = 
              buildGrids (i+1, j) g' ++ (buildGrids (i-1, j) g')
              ++ (buildGrids (i, j+1) g') ++ (buildGrids (i, j-1) g')
          where
            el = fromJust $ Map.lookup (i, j) g
            g' = Map.insert (i, j) 1 g
      intake = fst $ head $ Map.toList $ Map.filter (==2) fstG 
      memoizedBuildGrids = Memo.memo2 pairsMemo gridMemo buildGrids

numberOfCorrect :: [Grid] -> Int
numberOfCorrect = length . filter not . map correct
    where correct = elem 0 . map snd . Map.toList

toGrid :: (Int, Int) -> [Int] -> Grid
toGrid (maxW, maxH) = Map.fromList . zip ks
    where ks = [(x,y) | x <- [0..maxH-1], y <- [0..maxW-1]]

main = do
  l <- getLine
  let nums = map read $ words l :: [Int]
      maxW = nums !! 0
      maxH = nums !! 1
  print $ numberOfCorrect 
       $ toAcGrids (maxW, maxH) (toGrid (maxW, maxH) (drop 2 nums))
