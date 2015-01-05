{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.Trans.State as StateT
import Control.Monad.Trans
import Control.Monad.State.Lazy as State

import GUIState
import Board
import MyBoard

main :: IO ()
main = do 
	initGUI
	guiState <- newGUIState board
	runStateT runGUI guiState >> return ()
	mainGUI
	where board = initialize 54 (5,5) (0,0)

runGUI :: StateT GUIState IO ()
runGUI = do
	guiState <- StateT.get
	refreshTable
	liftIO $ onDestroy (window guiState) mainQuit
	return ()

refreshTable :: StateT GUIState IO ()
refreshTable = do
	guiState <- StateT.get
	let oldTable = table guiState
	let w = window guiState
	newTable <- myBoardToTable
	StateT.put $ setTable newTable guiState
	liftIO $ widgetDestroy oldTable
	liftIO $ GTK.set w [ containerBorderWidth := 10, containerChild := newTable ]
	liftIO $ widgetShowAll w


myBoardToTable :: StateT GUIState IO Table
myBoardToTable = do
	guiState <- StateT.get
	let b = board guiState
	table <- liftIO $ tableNew (width b) (height b) True
	cellsToTable 0 (val b) table


cellsToTable :: Int -> [[Cell]] -> Table -> StateT GUIState IO Table
cellsToTable _ [] table = return table
cellsToTable i (xs:xss) table = do
	newTable <- cellsToRow (i, 0) xs table
	cellsToTable (i+1) xss newTable

cellsToRow :: (Int, Int) -> [Cell] -> Table -> StateT GUIState IO Table
cellsToRow _ [] table = return table
cellsToRow (i, j) (x:xs) table = do
	button <- liftIO $ cellToButton x
	tableOutIO <- cellsToRow (i, (j+1)) xs table
	guiState <- State.get
	StateT.put $ setTable tableOutIO guiState
	liftIO $ onClicked button $ onClickedCell click (i, j) guiState
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

-- cell clicked event
-- because it is in the IO monad, it must get the state as a parameter to modify it
onClickedCell :: ((Int, Int) -> MyBoard -> MyBoard) -> (Int, Int) -> GUIState -> IO ()
onClickedCell f position guiState = do
	(x, y) <- runStateT (updateTable f position) guiState
	return x

-- called from onClickedCell to return in StateT monad and modify the state
updateTable :: ((Int, Int) -> MyBoard -> MyBoard) -> (Int, Int) -> StateT GUIState IO ()
updateTable f (i,j) = do
	guiState <- StateT.get
	let b = board guiState
	let newBoard = f (i,j) b
	StateT.put (setBoard newBoard guiState)
	refreshTable
	fmap (\b -> ()) StateT.get