module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

-- Define the win message
winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

-- Custom parsing functions
parseDirection :: String -> Maybe Direction
parseDirection str = case str of
    "North" -> Just North
    "South" -> Just South
    "East"  -> Just East
    "West"  -> Just West
    _       -> Nothing

parseGameObj :: String -> Maybe GameObj
parseGameObj str = case str of
    "Coffee" -> Just Coffee
    "Mug"    -> Just Mug
    "Door"   -> Just Door
    "Key"    -> Just Key
    _        -> Nothing

-- Process a command and return a new game state and a message for the user
process :: GameData -> [String] -> (GameData, String)
process state (cmd:args) =
    case parseCommand cmd args of
        Just command -> command state
        Nothing -> (state, "I don't understand that command.")
  where
    parseCommand :: String -> [String] -> Maybe (GameData -> (GameData, String))
    parseCommand "go" [dir] = fmap go (parseDirection dir)
    parseCommand "get" [obj] = fmap get (parseGameObj obj)
    parseCommand "drop" [obj] = fmap put (parseGameObj obj)
    -- Additional command parsing logic goes here...
    parseCommand _ _ = Nothing

-- REPL function to interact with the game
repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do
    print state
    putStr "What now? "
    hFlush stdout
    input <- getLine
    let (state', msg) = process state (words input)
    putStrLn msg
    if won state' then putStrLn winmessage >> return state'
    else repl state'

-- Main function
main :: IO ()
main = do
    repl initState
    return ()
