module Main (main) where

import Board
import MyBoard

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

main :: IO()
main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBoard)
