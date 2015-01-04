{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Board
import MyBoard
import Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.Trans.State as StateT
import Control.Monad.Trans
import Control.Monad.State.Lazy as State

main :: IO ()
main = do 
	initGUI
	runStateT launch (initialize 54 (5,5) (0,0)) >> return ()
	mainGUI

launch :: StateT MyBoard IO ()
launch = do
	window <- liftIO $ windowNew
	table <- myBoardToTable
	liftIO $ GTK.set window [ containerBorderWidth := 10, containerChild := table ]
	liftIO $ onDestroy window mainQuit
	liftIO $ widgetShowAll window
	return ()

myBoardToTable :: StateT MyBoard IO Table
myBoardToTable = do
	board <- State.get
	table <- liftIO $ tableNew (width board) (height board) True
	cellsToTable 0 (val board) table 


cellsToTable :: Int -> [[Cell]] -> Table -> StateT MyBoard IO Table
cellsToTable _ [] table = return table
cellsToTable i (xs:xss) table = do
	newTable <- cellsToRow (i, 0) xs table
	cellsToTable (i+1) xss newTable

cellsToRow :: (Int, Int) -> [Cell] -> Table -> StateT MyBoard IO Table
cellsToRow _ [] table = return table
cellsToRow (i, j) (x:xs) table = do
	button <- liftIO $ cellToButton x
	tableOutIO <- cellsToRow (i, (j+1)) xs table
	board <- State.get
	liftIO $ onClicked button $ onClickedCell (i, j) board
	liftIO $ tableAttachDefaults tableOutIO button i (i+1) j (j+1)
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

onClickedCell :: (Int, Int) -> MyBoard -> IO ()
onClickedCell position board = do
	(x, y) <- runStateT (changeState (click position)) board
	return x
	putStrLn $ show y

changeState :: (MyBoard -> MyBoard) -> StateT MyBoard IO ()
changeState f = do
	StateT.modify f
	fmap (\b -> ()) State.get