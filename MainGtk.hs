{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Graphics.UI.Gtk as GTK
import Data.IORef
import Control.Monad.Trans

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
	buttonSmiley :: Button,
	board :: MyBoard,
	buttons :: [[Button]],
	options :: Options
}

dummyProgramState :: IO ProgramState
dummyProgramState = do
	mainW <- windowNew
	optionsW <- windowNew
	button <- buttonNew
	let b = initialize 0 (0,0) (0,0)
	let opt = Options 0 (0,0) (0,0)
	return $ ProgramState mainW optionsW button b [[]] opt

setBoard :: MyBoard -> ProgramState -> ProgramState
setBoard b ps = ProgramState (mainWindow ps) (optionsWindow ps) (buttonSmiley ps) b (buttons ps) (options ps)

setButtons :: [[Button]] -> ProgramState -> ProgramState
setButtons b ps = ProgramState (mainWindow ps) (optionsWindow ps) (buttonSmiley ps) (board ps) b (options ps)

setOptions :: Options -> ProgramState -> ProgramState
setOptions opt ps = ProgramState (mainWindow ps) (optionsWindow ps) (buttonSmiley ps) (board ps) (buttons ps) opt

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

-------- update GUI methods --------
-- game
resetGame :: IORef ProgramState -> IO ()
resetGame ref = do
	ps <- readIORef ref
	let opt = options ps
	writeIORef ref $ setBoard (initialize (seed opt) (size opt) (firstClick opt)) ps
	updateTable ref
	setStateIngame ref

-- table
updateTable :: IORef ProgramState -> IO ()
updateTable ref = do
	ps <- readIORef ref
	let buttonTable = buttons ps
	let b = val $ board ps
	updateRow 0 b buttonTable

updateRow :: Int -> [[Cell]] -> [[Button]] -> IO ()
updateRow _ [] _ = return ()
updateRow i (xs:xss) buttonTable = do
	updateRow (i+1) xss buttonTable
	updateCell (i, 0) xs buttonTable

updateCell :: (Int, Int) -> [Cell] -> [[Button]] -> IO ()
updateCell _ [] _ = return ()
updateCell (i,j) (x:xs) buttonTable = do
	updateCell (i, (j+1)) xs buttonTable
	cellToButton x ((buttonTable!!i)!!j)

cellToButton :: Cell -> Button -> IO ()
cellToButton (Masked _) button = do
	emptyButton button
	image <- imageNewFromFile "masked.png"
	buttonSetImage button image
	buttonSetLabel button ""
cellToButton (Flagged _) button = do
	emptyButton button
	image <- imageNewFromFile "flag.png"
	buttonSetImage button image
	buttonSetLabel button ""
cellToButton (Clicked (-1)) button = do
	emptyButton button
	image <- imageNewFromFile "mine.png"
	buttonSetImage button image
	buttonSetLabel button ""
cellToButton (Clicked x) button = do
	emptyButton button
	buttonSetLabel button (show x)

activateTable :: IORef ProgramState -> IO ()
activateTable ref = do
	ps <- readIORef ref
	let buttonTable = buttons ps
	return $ map (map (flip widgetSetSensitive True)) buttonTable
	return ()

deActivateTable :: IORef ProgramState -> IO ()
deActivateTable ref = do
	ps <- readIORef ref
	let buttonTable = buttons ps
	return $ map (map $ flip widgetSetSensitive False) buttonTable
	return ()

emptyButton :: Button -> IO ()
emptyButton button = do
	children <- containerGetChildren button
	containerForeach button (containerRemove button)

-- button smile
setStateWon :: IORef ProgramState -> IO ()
setStateWon ref = do
	setButtonSmileImage "smiley_won.jpg" ref
	deActivateTable ref

setStateIngame :: IORef ProgramState -> IO ()
setStateIngame ref = do
	setButtonSmileImage "smiley_ingame.jpg" ref
	activateTable ref

setStateLost :: IORef ProgramState -> IO ()
setStateLost ref = do
	setButtonSmileImage "smiley_lost.jpg" ref
	deActivateTable ref

setButtonSmileImage :: String -> IORef ProgramState -> IO ()
setButtonSmileImage filename ref = do
	ps <- readIORef ref
	let buttonSmile = buttonSmiley ps
	emptyButton buttonSmile
	image <- imageNewFromFile filename
	buttonSetImage buttonSmile image


-------- build GUI methods --------
buildMainWindow :: IORef ProgramState -> IO Window
buildMainWindow ref = do
	ps <- readIORef ref
	let w = mainWindow ps
	-- button smiley
	let buttonSmile = buttonSmiley ps
	onClicked buttonSmile $ resetGame ref
	-- vbox
	vbox <- vBoxNew False 0
	GTK.set w [ containerChild := vbox ]
	containerAdd vbox buttonSmile
	onDestroy w mainQuit
	return w

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
		let opt = Options (read seed) (read size) (read click)
		let b = initialize (read seed) (read size) (read click)
		let newPS = setBoard b (setOptions opt ps)
		writeIORef ref newPS
		buildTable ref
		setStateIngame ref
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

buildTable :: IORef ProgramState -> IO ()
buildTable ref = do
	ps <- readIORef ref
	let b = board ps
	table <- tableNew (width b) (height b) True
	let w = mainWindow ps
	Just vbox <- binGetChild w
	containerAdd (castToContainer vbox) table
	buttonTable <- cellsToTable 0 (val b) table ref
	writeIORef ref $ setButtons buttonTable ps
	updateTable ref

cellsToTable :: Int -> [[Cell]] -> Table -> IORef ProgramState -> IO [[Button]]
cellsToTable _ [] _ _ = return []
cellsToTable i (xs:xss) table ref = do
	buttonTable <- cellsToTable (i+1) xss table ref
	buttonList <- cellsToRow (i, 0) xs table ref
	let newButtonTable = buttonList : buttonTable
	return newButtonTable

cellsToRow :: (Int, Int) -> [Cell] -> Table -> IORef ProgramState -> IO [Button]
cellsToRow _ [] _ _ = return []
cellsToRow (i, j) (x:xs) table ref = do
	buttonList <- cellsToRow (i, (j+1)) xs table ref
	button <- buttonNew
	button `on` buttonPressEvent $ tryEvent $ do
		LeftButton <- eventButton
		onClickedCell (click (i,j)) ref
	button `on` buttonPressEvent $ tryEvent $ do
		RightButton <- eventButton
		onClickedCell (flag (i,j)) ref
	tableAttachDefaults table button i (i+1) j (j+1)
	let newButtonList = button : buttonList
	return newButtonList

onClickedCell :: (MyBoard -> MyBoard) -> IORef ProgramState -> EventM EButton ()
onClickedCell callback ref = do
	ps <- liftIO $ readIORef ref
	let newBoard = callback (board ps)
	liftIO $ writeIORef ref $ setBoard newBoard ps
	liftIO $ updateTable ref
	liftIO $ if won newBoard then setStateWon ref else return ()
	liftIO $ if lost newBoard then setStateLost ref else return ()

