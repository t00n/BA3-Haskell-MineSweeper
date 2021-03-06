{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Board where

-- The show instance must be highly customized to display a board in ASCII
class Show b => Board b a | b -> a where
  -- Create a board from seed, dimension and first click (avoid immediate loosing)
  initialize :: Int -> (Int, Int) -> (Int, Int) -> b
  -- Click a cell on the board (no effect if out-of-bounds)
  click :: (Int,Int) -> b -> b
  -- Flag a cell on the board (no effect if out-of-bounds)
  flag :: (Int,Int) -> b -> b
  -- Test if all the mines have been flagged and all the clean cells clicked 
  won :: b -> Bool
  -- Test if a mined cell has been clicked
  lost :: b -> Bool
  -- Get value of a cell (not secured)
  get :: (Int, Int) -> b -> a
  -- Set a cell on the board with a value (no effect if out-of-bounds)
  set :: (Int, Int) -> a -> b -> b
  -- Reveal the entire board at the end of game
  reveal :: b -> b