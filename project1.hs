module Main (main) where

--added
import Data.Foldable as DFold (toList, foldr)
import Data.Sequence as DSeq
import Control.Monad (join)
--


-- The show instance must be highly customized to display a board in ASCII
class Show b => Board b where
  -- Create a board from seed, dimension and first click (avoid immediate loosing)
  initialize :: Int -> (Int, Int) -> (Int, Int) -> b
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
top :: Board b => (Int -> (Int, Int) -> (Int, Int) -> b) -> IO ()
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
loop :: Board b => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\n Gratz, you won!!!"
  | lost board = putStrLn $ show board ++ "\n Soz, you lost..."
  | otherwise  = do putStrLn $ show board
                    flag_loop (Just (-1, -1)) board
                    putStrLn "Click..."
                    coord <- readLn
                    loop $ click coord board

-- Place flags
flag_loop :: Board b => Maybe (Int, Int) -> b -> IO b
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
data MyBoard = MyBoard { val :: Seq (Seq Cell) }

instance Show Cell where
  show (Flagged _) = "F"
  show (Clicked x)
    | x == -1 = "M"
    | otherwise = show x
  show (Masked _) = " "

instance Show MyBoard where
  show (MyBoard s) 
    | DSeq.null s = ""
    | otherwise = (concat $ Prelude.replicate (DSeq.length xs) "+-") ++ "\n" -- boundaries
                  ++ (concat $ ["|" ++ (show x) | x <- (toList xs)]) ++ "\n" -- values for this line
                  ++ (show (MyBoard xss)) -- rest of the board
                  where xs = index s 0
                        xss = DSeq.drop 1 s

instance Board MyBoard where
  initialize seed (x,y) (c1,c2) = MyBoard $ DSeq.replicate x (DSeq.replicate y (Masked False))
    where nbOfMines = x*y `div` 10
  click (c1,c2) b = MyBoard $ newMatrix
    where oldColumn = index (val b) c1
          oldValue = index oldColumn c2
          newValue (Masked True) = (Clicked (-1))
          newValue (Masked False) = (Clicked nbOfAdjacentMines)
          newValue _ = oldValue
          nbOfAdjacentMines = 0
          newColumn = update c2 (newValue oldValue) oldColumn
          newMatrix = update c1 newColumn $ val b
  flag (f1,f2) b = b
  won b = DFold.foldr wonCell True $ join $ val b
    where wonCell (Flagged m) acc = acc && m == True
          wonCell (Masked _) _ = False
          wonCell (Clicked x) acc = acc && x >= 0
  lost b = DFold.foldr lostCell False $ join $ val b
    where lostCell (Flagged _) acc = acc || False
          lostCell (Masked _) acc = acc || False
          lostCell (Clicked a) acc  = acc || (a == -1)

main :: IO()
main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBoard)

