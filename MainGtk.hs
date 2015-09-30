{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Main (main) where

import Graphics.UI.Gtk as GTK
import Data.IORef
import Control.Monad.Trans

import Board
import MyBoard
import ProgramState

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
-- called when the smiley button is clicked
-- initialize the game board and the table according to the options in the state
resetGame :: IORef ProgramState -> IO ()
resetGame ref = do
	ps <- readIORef ref
	let opt = options ps
	writeIORef ref $ setBoard (initialize (seed opt) (size opt) (firstClick opt)) ps
	updateTable ref
	setStateIngame ref

-- called when the game is won
-- deactivates the table
-- changes the image on the smiley button
setStateWon :: IORef ProgramState -> IO ()
setStateWon ref = do
	setButtonSmileImage "smiley_won.jpg" ref
	deActivateTable ref

-- called when the game starts or is reset
-- activates the table
-- changes the image on the smiley button
-- resets the timer
-- sets the number of mines label
setStateIngame :: IORef ProgramState -> IO ()
setStateIngame ref = do
	setButtonSmileImage "smiley_ingame.jpg" ref
	initNbOfMines ref
	resetTimer ref
	activateTable ref

-- called when the game is lost
-- deactivates the table to prevent any further clicks
-- change the image on the smiley button to a sad smiley
-- reveals the entire board to the user
setStateLost :: IORef ProgramState -> IO ()
setStateLost ref = do
	ps <- readIORef ref
	let b = board ps
	writeIORef ref $ setBoard (reveal b) ps
	updateTable ref
	setButtonSmileImage "smiley_lost.jpg" ref
	deActivateTable ref

-------- update GUI methods --------
-- show the main window and hide the options window
showMainWindow :: IORef ProgramState -> IO ()
showMainWindow ref = do
	ps <- readIORef ref
	widgetShowAll (mainWindow ps)
	widgetHide (optionsWindow ps)

-- show the options window and hide the main window
showOptionsWindow :: IORef ProgramState -> IO ()
showOptionsWindow ref = do
	ps <- readIORef ref
	widgetShowAll (optionsWindow ps)
	widgetHide (mainWindow ps)

-- set the value ot the number of mines label to the actual number of mines in the game board
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

-- apply a function on the value of the number of mines label
modifyNbOfMines :: (Int -> Int) -> IORef ProgramState -> IO ()
modifyNbOfMines f ref = do
	ps <- readIORef ref
	let labelMines = nbOfMines ps
	text <- labelGetText labelMines
	let n = read text
	labelSetText labelMines $ show (f n)

-- change the image of the big smiley button in the info bar
-- takes a path to the file of the image as a parameter
setButtonSmileImage :: String -> IORef ProgramState -> IO ()
setButtonSmileImage filename ref = do
	ps <- readIORef ref
	let buttonSmile = buttonSmiley ps
	containerForeach buttonSmile (containerRemove buttonSmile)
	image <- imageNewFromFile filename
	buttonSetImage buttonSmile image

-- set the timer label to 0 and remove the timer function from the state
resetTimer :: IORef ProgramState -> IO ()
resetTimer ref = do
	ps <- readIORef ref
	let label = labelTimer ps
	labelSetText label "0"
	let function = functionTimer ps
	timeoutRemove function
	writeIORef ref $ setTimerFunction 0 ps
	return ()

-- start a timer function if one does not exist yet
startTimer :: IORef ProgramState -> IO ()
startTimer ref = do
	ps <- readIORef ref
	when (functionTimer ps) == 0 $ do
		function <- timeoutAdd (updateTimer ref) 1000
		writeIORef ref $ setTimerFunction function ps

-- function called each second during a game to increment the timer
updateTimer :: IORef ProgramState -> IO Bool
updateTimer ref = do
	ps <- readIORef ref
	let label = labelTimer ps
	labelText <- labelGetText label
	labelSetText label $ show $ (read labelText)+1
	let b = board ps
	return $ not $ (won b) || (lost b)


-- update the label and/or the image of each button in the table
-- according to the matching cell in the game board
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

-- change the label and/or the image of a button according to a cell
cellToButton :: Cell -> Button -> IO ()
cellToButton (Masked _) button = do
	image <- imageNewFromFile "masked.png"
	buttonSetImage button image
	buttonSetLabel button ""
cellToButton (Flagged _) button = do
	image <- imageNewFromFile "flag.png"
	buttonSetImage button image
	buttonSetLabel button ""
cellToButton (Clicked (-1)) button = do
	image <- imageNewFromFile "mine.png"
	buttonSetImage button image
	buttonSetLabel button ""
cellToButton (Clicked x) button = do
	buttonSetLabel button (show x)

-- called on the beginning of a new game
-- activate all buttons in the table
activateTable :: IORef ProgramState -> IO ()
activateTable ref = do
	ps <- readIORef ref
	let buttonTable = buttons ps
	mapM_ (mapM (flip widgetSetSensitive True)) buttonTable

-- called on the end of game (won or lost)
-- grey out all the buttons in the table
deActivateTable :: IORef ProgramState -> IO ()
deActivateTable ref = do
	ps <- readIORef ref
	let buttonTable = buttons ps
	mapM (mapM $ flip widgetSetSensitive False) buttonTable
	return ()

-------- build GUI methods --------
-- The main window contains a menu bar, an info bar and the table
buildMainWindow :: IORef ProgramState -> IO ()
buildMainWindow ref = do
	ps <- readIORef ref
	-- menu
	menuBar <- buildMenu ref
	-- info bar
	infoBox <- hBoxNew False 0
	onClicked (buttonSmiley ps) $ resetGame ref
	containerAdd infoBox (nbOfMines ps)
	containerAdd infoBox (buttonSmiley ps)
	containerAdd infoBox (labelTimer ps)
	-- game table
	table <- tableNew 0 0 True
	-- add everything in window
	vbox <- vBoxNew False 0
	containerAdd vbox menuBar
	containerAdd vbox infoBox
	containerAdd vbox table
	let w = mainWindow ps
	GTK.set w [ containerChild := vbox ]
	onDestroy w mainQuit
	return ()

-- build menu and returns the menu bar
buildMenu :: IORef ProgramState -> IO MenuBar
buildMenu ref = do
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
	return menuBar

-- The options window ask for a seed, the size of the board 
-- and the first click and put them in the state
buildOptionsWindow :: IORef ProgramState -> IO ()
buildOptionsWindow ref = do
	-- init
	ps <- readIORef ref
	let w = optionsWindow ps
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
		writeIORef ref $ setBoard b (setOptions opt ps)
		buildTable ref
		setStateIngame ref
		showMainWindow ref
	-- put objects in container
	vbox <- vBoxNew False 0
	containerAdd vbox labelSeed
	containerAdd vbox entrySeed
	containerAdd vbox labelSize
	containerAdd vbox entrySize
	containerAdd vbox labelClick
	containerAdd vbox entryClick
	containerAdd vbox buttonOk
	GTK.set w [containerChild := vbox ]
	onDestroy w mainQuit
	return ()

-- builds the table and destroy the old one
buildTable :: IORef ProgramState -> IO ()
buildTable ref = do
	ps <- readIORef ref
	let b = board ps
	-- destroy old table
	Just vbox <- binGetChild (mainWindow ps)
	children <- containerGetChildren (castToContainer vbox)
	widgetDestroy $ head $ reverse $ children
	-- create new table
	table <- tableNew (width b) (height b) True
	containerAdd (castToContainer vbox) table
	buttonTable <- cellsToTable 0 (val b) table ref
	modifyIORef ref $ setButtons buttonTable
	updateTable ref

cellsToTable :: Int -> [[Cell]] -> Table -> IORef ProgramState -> IO [[Button]]
cellsToTable _ [] _ _ = return []
cellsToTable i (xs:xss) table ref = do
	buttonTable <- cellsToTable (i+1) xss table ref
	buttonList <- cellsToRow (i, 0) xs table ref
	return $ buttonList : buttonTable

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
	return $ button : buttonList

-- function called when a button in the table is clicked
-- the callback is either the click function or the flag function
onClickedCell :: (MyBoard -> MyBoard) -> IORef ProgramState -> EventM EButton ()
onClickedCell f ref = do
	ps <- liftIO $ readIORef ref
	let newBoard = f (board ps)
	liftIO $ modifyIORef ref $ setBoard newBoard
	liftIO $ if won newBoard then setStateWon ref else do
		updateTable ref
		startTimer ref
	liftIO $ if lost newBoard then setStateLost ref else do
		updateTable ref
		startTimer ref

