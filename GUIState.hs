module GUIState where

import Graphics.UI.Gtk
import MyBoard

data GUIState = GUIState {
	board :: MyBoard,
	window :: Window,
	table :: Table
}

newGUIState :: MyBoard -> IO GUIState
newGUIState board = do
	window <- windowNew
	table <- tableNew (width board) (height board) True
	return $ GUIState board window table

setBoard :: MyBoard -> GUIState -> GUIState
setBoard newBoard guiState = GUIState newBoard (window guiState) (table guiState)

setTable :: Table -> GUIState -> GUIState
setTable newTable guiState = GUIState (board guiState) (window guiState) newTable