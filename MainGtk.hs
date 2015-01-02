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
	cellsToTable (val board) table
	where table = tableNew (width board) (height board) True

cellsToTable :: [[Cell]] -> IO Table -> IO Table
cellsToTable [] table = table
cellsToTable (xs:xss) table = do
	cellsToTable xss newTable
	where newTable = cellsToRow (length xss) xs table


cellsToRow :: Int -> [Cell] -> IO Table -> IO Table
cellsToRow _ [] table = table
cellsToRow n (x:xs) table = do
	button <- cellToButton x
	tableOutIO <- table
	tableAttachDefaults tableOutIO button (length xs) ((length xs)+1) n (n+1)
	cellsToRow n xs (return tableOutIO)


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