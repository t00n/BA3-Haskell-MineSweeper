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
	initWindowNew
	mainGUI

initWindowNew :: IO ()
initWindowNew = do
	-- init
	w <- windowNew
	vbox <- vBoxNew False 0
	labelSeed <- labelNew $ Just "Seed"
	entrySeed <- entryNew
	labelSize <- labelNew $ Just "Size"
	entrySize <- entryNew
	labelClick <- labelNew $ Just "First click"
	entryClick <- entryNew
	buttonOk <- buttonNewWithLabel "New"
	-- event callback
	onClicked buttonOk $ do
		seed <- entryGetText entrySeed :: IO [Char]
		size <- entryGetText entrySize :: IO [Char]
		click <- entryGetText entryClick :: IO [Char]
		guiState <- newGUIState $ initialize (read seed) (read size) (read click)
		runStateT runGUI guiState
		widgetHide w
		return ()
	-- put objects in container
	containerAdd vbox labelSeed
	containerAdd vbox entrySeed
	containerAdd vbox labelSize
	containerAdd vbox entrySize
	containerAdd vbox labelClick
	containerAdd vbox entryClick
	containerAdd vbox buttonOk
	-- window
	GTK.set w [containerChild := vbox ]
	onDestroy w mainQuit
	widgetShowAll w
	return ()

runGUI :: StateT GUIState IO ()
runGUI = do
	refreshTable
	guiState <- StateT.get
	liftIO $ onDestroy (window guiState) mainQuit
	return ()

refreshTable :: StateT GUIState IO ()
refreshTable = do
	guiState <- StateT.get
	let t = table guiState
	let w = window guiState
	newTable <- myBoardToTable
	StateT.modify $ setTable newTable
	liftIO $ widgetDestroy t
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
	newTable <- cellsToTable (i+1) xss table
	cellsToRow (i, 0) xs newTable

cellsToRow :: (Int, Int) -> [Cell] -> Table -> StateT GUIState IO Table
cellsToRow _ [] table = return table
cellsToRow (i, j) (x:xs) table = do
	tableOutIO <- cellsToRow (i, (j+1)) xs table
	StateT.modify $ setTable tableOutIO
	guiState <- State.get
	button <- liftIO $ cellToButton x
	liftIO $ button `on` buttonPressEvent $ tryEvent $ do
		LeftButton <- eventButton
		liftIO $ onClickedCell (click (i, j) (board guiState)) guiState
	liftIO $ button `on` buttonPressEvent $ tryEvent $ do
		RightButton <- eventButton
		liftIO $ onClickedCell (flag (i, j) (board guiState)) guiState
	liftIO $ tableAttachDefaults tableOutIO button i (i+1) j (j+1)
	return tableOutIO

-- create a gtk button from a cell
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
-- parameter 1 : the new board if this cell is clicked/flagged
-- parameter 2 : because it is in the IO monad, it must get the state as a parameter to modify it
onClickedCell :: MyBoard -> GUIState -> IO ()
onClickedCell b guiState = do
	(x, y) <- runStateT (updateTable b) guiState
	return x

-- called from onClickedCell to return in StateT monad and modify the state
updateTable :: MyBoard -> StateT GUIState IO ()
updateTable b = do
	StateT.modify $ setBoard b
	refreshTable
	guiState <- StateT.get
	fmap (\b -> ()) StateT.get