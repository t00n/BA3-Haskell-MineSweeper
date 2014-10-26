
module Project (Board(initialize, click, flag, won, lost), top) where

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

data Cell = Flagged | Clicked | Masked
data MyBoard = MyBoard [[Cell]]

instance Show MyBoard where
  show (MyBoard a) = show "caca"

instance Board MyBoard where
  initialize seed (x,y) (c1,c2) = MyBoard [[Masked]]
  click (c1,c2) b = b
  flag (f1,f2) b = b
  won b = False
  lost b = False

main :: IO()
main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBoard)

