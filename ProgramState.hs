module ProgramState where

import Graphics.UI.Gtk
import MyBoard
import Board

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
