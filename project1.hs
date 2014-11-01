module Main where

--added
import Data.Foldable as Fold (foldr)
import Data.Vector as Vec hiding ((++), concat, update, fromList)
import Control.Monad (join)
import System.Random
import Data.Set as Set (fromList, toList)
--


-- The show instance must be highly customized to display a board in ASCII
class Show b => Board b a | b -> a where
  -- Create a board from seed, dimension and first click (avoid immediate loosing)
  initialize :: Int -> (Int, Int) -> (Int, Int) -> b
  -- Get value of a cell (not secured)
  get :: (Int, Int) -> b -> a
  -- Update a cell on the board with a value (no effect if out-of-bounds)
  update :: (Int, Int) -> a -> b -> b
  -- Click a cell on the board (no effect if out-of-bounds)
  click :: (Int,Int) -> b -> b
  -- Flag a cell on the board (no effect if out-of-bounds)
  flag :: (Int,Int) -> b -> b
  -- Test if all the mines have been flagged and all the clean cells clicked 
  won :: b -> Bool
  -- Test if a mined cell has been clicked
  lost :: b -> Bool

-- Create a main function by given a your initialize implementation
-- e.g.: main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBoard)
top :: Board b a => (Int -> (Int, Int) -> (Int, Int) -> b) -> IO ()
top cinit = do putStrLn "Enter a seed..."
               seed <- readLn
               putStrLn "Enter the width of the board"
               width <- readLn
               putStrLn "Enter the height of the board"
               height <- readLn
               putStrLn "First click..."
               firstClick <- readLn -- modified
               loop $ cinit seed (width, height) firstClick -- modified
               -- should check if first click was good or not

-- A turn
loop :: Board b a => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\n Gratz, you won!!!"
  | lost board = putStrLn $ show board ++ "\n Soz, you lost..."
  | otherwise  = do putStrLn $ show board
                    board <- flag_loop (Just (-1, -1)) board -- modified
                    putStrLn "Click..."
                    coord <- readLn
                    loop $ click coord board

-- Place flags
flag_loop :: Board b a => Maybe (Int, Int) -> b -> IO b
flag_loop Nothing board = return board
flag_loop (Just coord) board = do putStrLn "Place a flag???"
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)

----------------------------------- My Code -----------------------------------

-- a cell is
-- flagged, mine or not
-- clicked, mine (-1) or number of adjacent mine
-- masked, mine or not
data Cell = Flagged Bool | Clicked Int | Masked Bool
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

removeDuplicates::Ord a => [(a, b)]->[(a, b)]
removeDuplicates theList = [x|(Just x, _, _) <- iterate getNext (Nothing, theList, [])]
  where 
    getNext (_, x:xs, used) 
      | fst x `Prelude.elem` used = (Nothing, xs, used)
      | otherwise = (Just x, xs, (fst x):used)

uniqueRandomInts :: (RandomGen g) => (Int, Int) -> Int -> g -> [Int]
uniqueRandomInts range n g = Prelude.map fst . Prelude.take n . removeDuplicates . iterate getNext $ randomR range g
  where getNext = randomR range . snd


instance Board MyBoard Cell where
  initialize seed (x,y) (c1,c2) = MyBoard s x y
    where nbOfMines = x*y `div` 6 + 1
          gen = uniqueRandomInts (0, x*y-1) nbOfMines (mkStdGen seed)
          s = generate (x*y-1) placeMine
          placeMine i
            | i `Prelude.elem` gen && i /= (c1*x + c2) = (Masked True)
            | otherwise = (Masked False)
          --s = Prelude.foldr (\x xs -> xs // [(x, (Masked True))]) (Vec.replicate (x*y-1) (Masked False)) gen
  get (x, y) b = (val b) ! (x*w + y)
    where w = width b
  update (x, y) a b 
    | x < 0 || y < 0 || x >= (width b) || y >= (height b) = b
    | otherwise = MyBoard ((val b) // [(i, a)]) w h
      where w = width b
            h = height b
            i = x*w + y
  click (x, y) b = update (x, y) (newValue oldValue) b
    where oldValue = get (x, y) b
          newValue (Masked True) = (Clicked (-1))
          newValue (Masked False) = (Clicked nbOfAdjacentMines)
          newValue _ = oldValue
          nbOfAdjacentMines = 0
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

main :: IO()
main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBoard)

