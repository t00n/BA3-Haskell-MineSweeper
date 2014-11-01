module MyBoard where

import Data.Foldable as Fold (foldr)
import Data.Vector as Vec hiding ((++), concat, update)
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

instance Show MyBoard where
  show b 
    | height b == 0 = ""
    | otherwise = (concat $ Prelude.replicate w "+-") ++ "\n" -- boundaries
                  ++ (concat $ Vec.toList $ Vec.map ("|" ++) $ Vec.map show xs) ++ "\n" -- values for this line
                  ++ (show (MyBoard xss w newH)) -- rest of the board
                  where (xs, xss) = Vec.splitAt w (val b)
                        w = width b
                        newH = (height b)-1

instance Board MyBoard Cell where
  initialize seed (x,y) (c1,c2) = click (c1, c2) (MyBoard s x y)
    where nbOfMines = x*y `div` 4
          firstClick = c1*x + c2
          size = x*y-1
          range = (0, size)
          removeDuplicates aList = [x|(Just x, _, _) <- iterate getNext (Nothing, aList, [])]
            where getNext (_, x:xs, used) 
                    | fst x `Prelude.elem` used || fst x == firstClick = (Nothing, xs, used)
                    | otherwise = (Just x, xs, (fst x):used)
          uniqueRandomInts = Prelude.map fst . Prelude.take nbOfMines . removeDuplicates . iterate getNext . randomR range
            where getNext = randomR range . snd
          gen = uniqueRandomInts (mkStdGen seed)
          s = generate size (\i -> if i `Prelude.elem` gen then (Masked True) else (Masked False))
  get (x, y) b = (val b) ! (x*w + y)
    where w = width b
  update (x, y) a b 
    | x < 0 || y < 0 || x >= (width b) || y >= (height b) = b
    | otherwise = MyBoard ((val b) // [(i, a)]) w h
      where w = width b
            h = height b
            i = x*w + y
  click (x, y) b 
    | nbOfAdjacentMines /= 0 = update (x, y) (newValue oldValue) b
    | otherwise = Fold.foldr (\x acc -> click x acc) b neighbours
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