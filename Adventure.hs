-- This is the main module where the program execution begins.
module Main where

-- Importing necessary modules.
import World   --  contains the game world's data structures and logic.
import Actions --  includes definitions for game actions.

-- Importing libraries for monads, input/output, and system interaction.
import Control.Monad
import System.IO
import System.Exit

-- Define the winning message displayed when the game is won.
winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

-- Custom function to parse a string into a `Direction` data type.
-- Just is similar to maybe
parseDirection :: String -> Maybe Direction
parseDirection str = case str of
    "North" -> Just North  -- If the string is "North", return `North`.
    "South" -> Just South  -- If the string is "South", return `South`.
    "East"  -> Just East   -- If the string is "East", return `East`.
    "West"  -> Just West   -- If the string is "West", return `West`.
    _       -> Nothing     -- If the string doesn't match, return `Nothing`.

-- Custom function to parse a string into a `GameObj` data type.
parseGameObj :: String -> Maybe GameObj
parseGameObj str = case str of
    "Coffee" -> Just Coffee -- If the string is "Coffee", return `Coffee`.
    "Mug"    -> Just Mug    -- If the string is "Mug", return `Mug`.
    "Door"   -> Just Door   -- If the string is "Door", return `Door`.
    "Key"    -> Just Key    -- If the string is "Key", return `Key`.
    _        -> Nothing     -- If the string doesn't match, return `Nothing`.

-- Processes a command and returns a new game state and a message for the user.
process :: GameData -> [String] -> (GameData, String)
process state (cmd:args) =
    case parseCommand cmd args of
        Just command -> command state    -- Execute the command if it's valid.
        Nothing -> (state, "I don't understand that command.") -- Error message for invalid command.
  where
    -- Parses a command string and its arguments into a game action.
    parseCommand :: String -> [String] -> Maybe (GameData -> (GameData, String))
    parseCommand "go" [dir] = fmap go (parseDirection dir)     -- Parses "go" command with direction.
    parseCommand "get" [obj] = fmap get (parseGameObj obj)     -- Parses "get" command with an object.
    parseCommand "drop" [obj] = fmap put (parseGameObj obj)    -- Parses "drop" command with an object.
    -- Additional command parsing logic can be added here.
    parseCommand _ _ = Nothing

-- REPL (Read-Eval-Print Loop) function for interacting with the game.
repl :: GameData -> IO GameData
repl state | finished state = return state  -- If the game is finished, return the state.
repl state = do
    print state  -- Print the current game state.
    putStr "What now? "
    hFlush stdout  -- Flush the output to ensure "What now?" is printed.
    input <- getLine  -- Get user input.
    let (state', msg) = process state (words input)  -- Process the input.
    putStrLn msg  -- Print the output message.
    if won state' then putStrLn winmessage >> return state'  -- Check if the game is won and print the win message.
    else repl state'  -- Continue the REPL loop.

-- The main function where the program starts.
main :: IO ()
main = do
    repl initState  -- Start the REPL loop with the initial game state.
    return ()
