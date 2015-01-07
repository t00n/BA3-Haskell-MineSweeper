{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Graphics.UI.Gtk as GTK
import Data.IORef

import Board
import MyBoard

data Options = Options {
	seed :: Int,
	size :: (Int, Int),
	firstClick :: (Int, Int)
}

data ProgramState = ProgramState {
	mainWindow :: Window,
	optionsWindow :: Window,
	board :: MyBoard,
	buttons :: [[Button]],
	options :: Options
}

dummyProgramState :: IO ProgramState
dummyProgramState = do
	mainW <- windowNew
	optionsW <- windowNew
	let b = initialize 0 (0,0) (0,0)
	let opt = Options 0 (0,0) (0,0)
	return $ ProgramState mainW optionsW b [[]] opt

main :: IO ()
main = do
	initGUI
	ps <- dummyProgramState
	ref <- newIORef ps
	buildOptionsWindow ref
	buildMainWindow ref
	showOptionsWindow ref
	mainGUI

showMainWindow :: IORef ProgramState -> IO ()
showMainWindow ref = do
	ps <- readIORef ref
	widgetShowAll (mainWindow ps)
	widgetHide (optionsWindow ps)

showOptionsWindow :: IORef ProgramState -> IO ()
showOptionsWindow ref = do
	ps <- readIORef ref
	widgetShowAll (optionsWindow ps)
	widgetHide (mainWindow ps)

buildMainWindow :: IORef ProgramState -> IO Window
buildMainWindow ref = do
	ps <- readIORef ref
	let w = mainWindow ps
	onDestroy w mainQuit
	return w

buildTable :: IORef ProgramState -> IO ()
buildTable ref = do
	ps <-readIORef ref
	let b = board ps
	table <- tableNew (width b) (height b) True
	cellsToTable 0 (val b) table
	let w = mainWindow ps
	GTK.set w [ containerChild := table ]

cellsToTable :: Int -> [[Cell]] -> Table -> IO ()
cellsToTable _ [] table = return ()
cellsToTable i (xs:xss) table = do
	cellsToTable (i+1) xss table
	cellsToRow (i, 0) xs table

cellsToRow :: (Int, Int) -> [Cell] -> Table -> IO ()
cellsToRow _ [] table = return ()
cellsToRow (i, j) (x:xs) table = do
	cellsToRow (i, (j+1)) xs table
	button <- buttonNew
	tableAttachDefaults table button i (i+1) j (j+1)

	--button `on` buttonPressEvent $ tryEvent $ do
	--	LeftButton <- eventButton
	--	onClickedCell (click (i, j) (board guiState)) guiState
	--button `on` buttonPressEvent $ tryEvent $ do
	--	RightButton <- eventButton
	--	onClickedCell (flag (i, j) (board guiState)) guiState

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

buildOptionsWindow :: IORef ProgramState -> IO Window
buildOptionsWindow ref = do
	-- init
	ps <- readIORef ref
	let w = optionsWindow ps
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
		let b = initialize (read seed) (read size) (read click)
		let newPS = ProgramState (mainWindow ps) (optionsWindow ps) b (buttons ps) (options ps)
		writeIORef ref newPS
		buildTable ref
		showMainWindow ref
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
	return w