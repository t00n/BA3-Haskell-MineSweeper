{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Board
import MyBoard
import Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.Trans.State as State
import Control.Monad.Trans

main :: IO ()
main = do 
	initGUI
	runStateT launch (initialize 54 (5,5) (0,0)) >> return ()
	mainGUI

launch :: StateT MyBoard IO ()
launch = do
	window <- liftIO $ windowNew
	board <- State.get
	table <- liftIO $ myBoardToTable board
	liftIO $ GTK.set window [ containerBorderWidth := 10, containerChild := table ]
	liftIO $ onDestroy window mainQuit
	liftIO $ widgetShowAll window
	return ()

myBoardToTable :: MyBoard -> IO Table
myBoardToTable board = do
	cellsToTable 0 (val board) table
	where table = tableNew (width board) (height board) True


cellsToTable :: Int -> [[Cell]] -> IO Table -> IO Table
cellsToTable _ [] table = table
cellsToTable i (xs:xss) table = do
	cellsToTable (i+1) xss newTable
	where newTable = cellsToRow (i, 0) xs table


cellsToRow :: (Int, Int) -> [Cell] -> IO Table -> IO Table
cellsToRow _ [] table = table
cellsToRow (i, j) (x:xs) table = do
	button <- cellToButton x
	tableOutIO <- cellsToRow (i, (j+1)) xs table
	tableAttachDefaults tableOutIO button i (i+1) j (j+1)
	return tableOutIO


cellToButton :: Cell -> IO Button
cellToButton (Masked _) = do
	button <- buttonNew
	image <- imageNewFromFile "masked.png"
	containerAdd button image
	return button
cellToButton (Flagged _) = do
	button <- buttonNew
	image <- imageNewFromFile "flag.png"
	containerAdd button image
	return button
cellToButton (Clicked (-1)) = do
	button <- buttonNew
	image <- imageNewFromFile "mine.png"
	containerAdd button image
	return button
cellToButton (Clicked x) = buttonNewWithLabel (show x)

createBoard :: StateT MyBoard IO Table
createBoard = do
	board <- State.get
	table <- liftIO $ tableNew (width board) (height board) True
	createCells table ((width board)*(height board))

createCells :: Table -> Int -> StateT MyBoard IO Table
createCells table 0 = return table
createCells table n = do
	board <- State.get
	button <- liftIO $ buttonNew
	liftIO $ tableAttach table button ((n `mod` (width board))-1) ((n `mod` (width board))) ((n `div` (width board))-1) (n `div` (width board)) [] [] 0 0
	createCells table (n-1)