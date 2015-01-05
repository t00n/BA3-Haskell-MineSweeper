{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Board
import MyBoard
import Graphics.UI.Gtk as GTK
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.Trans.State as StateT
import Control.Monad.Trans
import Control.Monad.State.Lazy as State

import GUIState

main :: IO ()
main = do 
	initGUI
	guiState <- newGUIState board
	runStateT runWindow guiState >> return ()
	mainGUI
	where board = initialize 54 (5,5) (0,0)

runWindow :: StateT GUIState IO ()
runWindow = do
	guiState <- StateT.get
	refreshTable
	liftIO $ onDestroy (window guiState) mainQuit
	return ()

refreshTable :: StateT GUIState IO ()
refreshTable = do
	newTable <- myBoardToTable
	guiState <- StateT.get
	StateT.put $ setTable newTable guiState
	liftIO $ GTK.set (window guiState) [ containerBorderWidth := 10, containerChild := newTable ]
	liftIO $ widgetShowAll (window guiState)


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
	liftIO $ onClicked button $ onClickedCell (i, j) guiState
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

onClickedCell :: (Int, Int) -> GUIState -> IO ()
onClickedCell position guiState = do
	(x, y) <- runStateT (changeState position click) guiState
	return x
	putStrLn $ show (board y)

changeState :: (Int, Int) -> ((Int, Int) -> MyBoard -> MyBoard) -> StateT GUIState IO ()
changeState (i,j) f = do
	guiState <- StateT.get
	let b = board guiState
	let newBoard = f (i,j) b
	StateT.put (setBoard newBoard guiState)
	let tableOutIO = table guiState
	let w = window guiState
	liftIO $ widgetDestroy tableOutIO
	liftIO $ putStrLn (show b)
	newTable <- myBoardToTable
	liftIO $ GTK.set w [ containerBorderWidth := 10, containerChild := newTable ]
	liftIO $ widgetShowAll w
	State.put (setTable tableOutIO guiState)
	fmap (\b -> ()) StateT.get