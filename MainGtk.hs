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
	nbOfMines :: Label,
	buttonSmiley :: Button,
	labelTimer :: Label,
	functionTimer :: HandlerId,
	board :: MyBoard,
	buttons :: [[Button]],
	options :: Options
}

dummyProgramState :: IO ProgramState
dummyProgramState = do
	mainW <- windowNew
	optionsW <- windowNew
	button <- buttonNew
	nbOfMines <- labelNew $ Just "0"
	labelTimer <- labelNew $ Just "0"
	let b = initialize 0 (0,0) (0,0)
	let opt = Options 0 (0,0) (0,0)
	return $ ProgramState mainW optionsW nbOfMines button labelTimer 0 b [[]] opt

setTimerFunction :: HandlerId -> ProgramState -> ProgramState
setTimerFunction f ps = ProgramState (mainWindow ps) (optionsWindow ps) (nbOfMines ps) (buttonSmiley ps) (labelTimer ps) f (board ps) (buttons ps) (options ps)

setBoard :: MyBoard -> ProgramState -> ProgramState
setBoard b ps = ProgramState (mainWindow ps) (optionsWindow ps) (nbOfMines ps) (buttonSmiley ps) (labelTimer ps) (functionTimer ps) b (buttons ps) (options ps)

setButtons :: [[Button]] -> ProgramState -> ProgramState
setButtons b ps = ProgramState (mainWindow ps) (optionsWindow ps) (nbOfMines ps) (buttonSmiley ps) (labelTimer ps) (functionTimer ps) (board ps) b (options ps)

setOptions :: Options -> ProgramState -> ProgramState
setOptions opt ps = ProgramState (mainWindow ps) (optionsWindow ps) (nbOfMines ps) (buttonSmiley ps) (labelTimer ps) (functionTimer ps) (board ps) (buttons ps) opt

main :: IO ()
main = do
	initGUI
	ps <- dummyProgramState
	ref <- newIORef ps
	buildOptionsWindow ref
	buildMainWindow ref
	showOptionsWindow ref
	mainGUI

-------- game methods --------
resetGame :: IORef ProgramState -> IO ()
resetGame ref = do
	ps <- readIORef ref
	let opt = options ps
	writeIORef ref $ setBoard (initialize (seed opt) (size opt) (firstClick opt)) ps
	updateTable ref
	setStateIngame ref

setStateWon :: IORef ProgramState -> IO ()
setStateWon ref = do
	setButtonSmileImage "smiley_won.jpg" ref
	deActivateTable ref

setStateIngame :: IORef ProgramState -> IO ()
setStateIngame ref = do
	setButtonSmileImage "smiley_ingame.jpg" ref
	initNbOfMines ref
	resetTimer ref
	activateTable ref

setStateLost :: IORef ProgramState -> IO ()
setStateLost ref = do
	ps <- readIORef ref
	let b = board ps
	writeIORef ref $ setBoard (reveal b) ps
	updateTable ref
	setButtonSmileImage "smiley_lost.jpg" ref
	deActivateTable ref

-------- update GUI methods --------
-- global
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

emptyButton :: Button -> IO ()
emptyButton button = do
	children <- containerGetChildren button
	containerForeach button (containerRemove button)

-- info box
initNbOfMines :: IORef ProgramState -> IO ()
initNbOfMines ref = do
	ps <- readIORef ref
	let b = board ps
	let labelMines = nbOfMines ps
	let minesTotal = foldr (\xs acc -> acc + foldr (\x acc -> if x == (Masked True) then (acc+1) else acc) 0 xs) 0 (val b)
	labelSetText labelMines (show minesTotal)

decNbOfMines :: IORef ProgramState -> IO ()
decNbOfMines ref = modifyNbOfMines (subtract 1) ref

incNbOfMines :: IORef ProgramState -> IO ()
incNbOfMines ref = modifyNbOfMines (+1) ref

modifyNbOfMines :: (Int -> Int) -> IORef ProgramState -> IO ()
modifyNbOfMines f ref = do
	ps <- readIORef ref
	let labelMines = nbOfMines ps
	text <- labelGetText labelMines
	let n = read text
	labelSetText labelMines $ show (f n)

setButtonSmileImage :: String -> IORef ProgramState -> IO ()
setButtonSmileImage filename ref = do
	ps <- readIORef ref
	let buttonSmile = buttonSmiley ps
	emptyButton buttonSmile
	image <- imageNewFromFile filename
	buttonSetImage buttonSmile image

resetTimer :: IORef ProgramState -> IO ()
resetTimer ref = do
	ps <- readIORef ref
	let label = labelTimer ps
	labelSetText label "0"
	let function = functionTimer ps
	timeoutRemove function
	writeIORef ref $ setTimerFunction 0 ps
	return ()

startTimer :: IORef ProgramState -> IO ()
startTimer ref = do
	ps <- readIORef ref
	if (functionTimer ps) == 0 then do
		function <- timeoutAdd (updateTimer ref) 1000
		writeIORef ref $ setTimerFunction function ps
	else return ()
	return ()

updateTimer :: IORef ProgramState -> IO Bool
updateTimer ref = do
	ps <- readIORef ref
	let label = labelTimer ps
	labelText <- labelGetText label
	labelSetText label $ show $ (read labelText)+1
	let b = board ps
	return $ not $ (won b) || (lost b)


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
	mapM (mapM (flip widgetSetSensitive True)) buttonTable
	return ()

deActivateTable :: IORef ProgramState -> IO ()
deActivateTable ref = do
	ps <- readIORef ref
	let buttonTable = buttons ps
	mapM (mapM $ flip widgetSetSensitive False) buttonTable
	return ()

-------- build GUI methods --------
buildMainWindow :: IORef ProgramState -> IO Window
buildMainWindow ref = do
	ps <- readIORef ref
	let w = mainWindow ps
	-- menu --
	menuBar <- menuBarNew
	gameMenuItem <- menuItemNewWithLabel "Game"
	containerAdd menuBar gameMenuItem
	gameMenu <- menuNew
	menuItemSetSubmenu gameMenuItem gameMenu
	newMenuItem <- menuItemNewWithLabel "New"
	containerAdd gameMenu newMenuItem
	on newMenuItem menuItemActivate (showOptionsWindow ref)
	quitMenuItem <- menuItemNewWithLabel "Quit"
	containerAdd gameMenu quitMenuItem
	on quitMenuItem menuItemActivate mainQuit
	-- infos --
	infoBox <- hBoxNew False 0
	-- nb of mines
	let labelMines = nbOfMines ps
	-- button smiley
	let buttonSmile = buttonSmiley ps
	onClicked buttonSmile $ resetGame ref
	-- timer
	let labelTime = labelTimer ps
	-- infoBox
	containerAdd infoBox labelMines
	containerAdd infoBox buttonSmile
	containerAdd infoBox labelTime
	-- table -- 
	table <- tableNew 0 0 True
	-- vbox --
	vbox <- vBoxNew False 0
	GTK.set w [ containerChild := vbox ]
	containerAdd vbox menuBar
	containerAdd vbox infoBox
	containerAdd vbox table
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
	let w = mainWindow ps
	Just vbox <- binGetChild w
	children <- containerGetChildren (castToContainer vbox)
	widgetDestroy $ head $ reverse $ children
	table <- tableNew (width b) (height b) True
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
		ps <- liftIO $ readIORef ref
		let cell = Board.get (i,j) (board ps)
		liftIO $ if cell == (Masked True) || cell == (Masked False) then decNbOfMines ref else return ()
		liftIO $ if cell == (Flagged True) || cell == (Flagged False) then incNbOfMines ref else return ()
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
	liftIO $ startTimer ref

