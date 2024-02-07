module Main where

import World    -- Contains the game world's data structures and logic.
import Actions  -- Includes definitions for game actions and helper functions.
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline
import Test.QuickCheck
import Test.QuickCheck.All

clear = "\ESC[2J"

winmessage = "Congratulations, you have made it out of the house.\nNow go to your lectures..."
headache = "You have a throbbing headache, take some paracetamol before you leave. ('swallow pill')\n"

-- Given a game state, and user input (as a list of words) return a new game state and a message for the user.
process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

-- REPL function to interact with the game, showing available actions and handling user input with Haskeline.
repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do
  outputStrLn "" -- Equivalent to `putStr "\n"`
  liftIO $ print state
  if (medicated state) then 
    outputStr ""
  else 
    outputStr headache
  outputStr "\nWhat now? "
  minput <- getInputLine ""
  outputStr clear -- Remove this line to turn off the automatic terminal clear after each input.
  case minput of
    Nothing -> return state -- Handle EOF or CTRL+D
    Just cmd -> do
      let (state', msg) = process state (words cmd)
      outputStrLn "" -- New line for clean separation
      outputStrLn msg
      if won state'
        then do outputStrLn winmessage
                return state'
        else repl state'

-- Main function to start the game loop.
main :: IO ()
main = runInputT defaultSettings (repl initState) >> return ()
