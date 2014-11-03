module MyBoard where

import Data.Foldable as Fold (foldr)
import Data.Vector as Vec hiding ((++), concat, update, replicate, take, elem, map)
import System.Random

import Board

-- a cell is
-- flagged, mine or not
-- clicked, mine (-1) or number of adjacent mine
-- masked, mine or not
data Cell = Flagged Bool | Clicked Int | Masked Bool deriving (Eq)

data MyBoard = MyBoard { 
  val :: Vector Cell,
  width :: Int,
  height :: Int
}

instance Show Cell where
  show (Flagged _) = "F"
  show (Clicked x)
    | x == -1 = "M"
    | otherwise = show x
  show (Masked False) = " "
  show (Masked True) = "M"

-- display a beautiful board
instance Show MyBoard where
  show b 
    | height b == 0 = ""
    | otherwise = (concat $ replicate w "+-") ++ "\n" -- boundaries
                  ++ (concat $ map ("|" ++) $ map show (toList xs)) ++ "\n" -- values for this line
                  ++ (show (MyBoard xss w newH)) -- rest of the board
                  where (xs, xss) = Vec.splitAt w (val b)
                        w = width b
                        newH = (height b)-1

-- remove duplicates and one specified exception in a iterate-generated list (one at a time)
removeDuplicates :: Eq a => a -> [(a, b)] -> [(a, b)]
removeDuplicates exception aList = [x|(Just x, _, _) <- iterate getNext (Nothing, aList, [])]
  where getNext (_, x:xs, used) 
          | fst x `elem` used || fst x == exception = (Nothing, xs, used)
          | otherwise = (Just x, xs, (fst x):used)

-- generate a list of random Ints of size "n" in range "range" excluding duplicates and "exception"
uniqueRandomInts :: (Random a, Eq a, RandomGen b) => a -> (a, a) -> Int -> b -> [a]
uniqueRandomInts exception range n = map fst . take n . removeDuplicates exception . iterate getNext . randomR range
  where getNext = randomR range . snd

instance Board MyBoard Cell where
  initialize seed (width,height) (c1,c2) = click (c1, c2) (MyBoard vec width height)
    where nbOfMines = width*height `div` 4 -- 4 is a magic number !
          firstClick = c1*width + c2
          sizeVec = width*height-1
          randomList = uniqueRandomInts firstClick (0, sizeVec) nbOfMines (mkStdGen seed)
          vec = generate sizeVec (\i -> if i `elem` randomList then (Masked True) else (Masked False))
  get (x, y) b = (val b) ! (x*w + y)
    where w = width b
  update (x, y) a b 
    | x < 0 || y < 0 || x >= w || y >= h = b
    | otherwise = MyBoard ((val b) // [(i, a)]) w h
      where w = width b
            h = height b
            i = x*w + y
  click (x, y) b
    | nbOfAdjacentMines /= 0 = update (x, y) (newValue oldValue) b
    | otherwise = Fold.foldr (\x acc -> click x acc) (update (x, y) (newValue oldValue) b) neighbours
      where oldValue = get (x, y) b
            newValue (Masked True) = (Clicked (-1))
            newValue (Masked False) = (Clicked nbOfAdjacentMines)
            newValue _ = oldValue
            nbOfAdjacentMines = Fold.foldr (\x acc -> if x == (Masked True) || x == (Flagged True) then acc+1 else acc) 0 [(val b) ! (i*w+j) | (i,j) <- neighbours]
            neighbours = [(i, j) | i <- [(x-1)..(x+1)], j <- [(y-1)..(y+1)], i >= 0, i < w, j >= 0, j < h, (i, j) /= (x,y)]
            w = width b
            h = height b
  flag (x, y) b = update (x, y) (newValue oldValue) b
    where oldValue = get (x, y) b
          newValue (Masked x) = (Flagged x)
          newValue (Flagged x) = (Masked x)
          newValue _ = oldValue
  won b = Fold.foldr wonCell True $ val b
    where wonCell (Flagged m) acc = acc && m == True
          wonCell (Masked _) _ = False
          wonCell (Clicked x) acc = acc && x >= 0
  lost b = Fold.foldr lostCell False $ val b
    where lostCell (Flagged _) acc = acc || False
          lostCell (Masked _) acc = acc || False
          lostCell (Clicked a) acc  = acc || (a == -1)