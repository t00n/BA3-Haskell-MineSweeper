{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

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

-- A turn
loop :: Board b a => b -> IO ()
loop board
  | won board  = putStrLn $ show (reveal board) ++ "\n Gratz, you won!!!"
  | lost board = putStrLn $ show (reveal board) ++ "\n Soz, you lost..."
  | otherwise  = do board <- flag_loop (Just (-1, -1)) board -- modified
                    putStrLn $ (show board) ++ "Click..." -- modified
                    coord <- readLn
                    loop $ click coord board

-- Place flags
flag_loop :: Board b a => Maybe (Int, Int) -> b -> IO b
flag_loop Nothing board = return board
flag_loop (Just coord) board = do putStrLn $ (show board) ++ "Place a flag???" -- modified
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)

main :: IO()
main = top (initialize :: Int -> (Int,Int) -> (Int,Int) -> MyBoard)

