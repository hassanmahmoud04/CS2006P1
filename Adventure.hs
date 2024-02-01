module Main where

import World    -- Contains the game world's data structures and logic.
import Actions  -- Includes definitions for game actions and helper functions.
import Control.Monad
import System.IO
import System.Exit
import Data.Char (toLower, isSpace) -- For case-insensitive input processing and trimming spaces.
import qualified World as W
import System.IO (hFlush, stdout)
import Actions (go, get, put)


-- Winning message displayed upon game completion.
winmessage = "Congratulations, you have made it out of the house.\nNow go to your lectures..."

-- Parses user input into a `Direction` data type, handling input case-insensitively.
parseDirection :: String -> Maybe Direction
parseDirection str = case map toLower str of
    "north" -> Just North
    "south" -> Just South
    "east"  -> Just East
    "west"  -> Just West
    _       -> Nothing

process :: W.GameData -> String -> (W.GameData, String)
process state input =
    let (cmd:rest) = words $ map toLower input
        action = unwords rest
    in case cmd of
        "go" -> maybe (state, "I don't understand that command.") (`go` state) (parseDirection action)
        "get" -> handleGetObject action state
        "drop" -> handleDropObject action state
        _ -> (state, "I don't understand that command.")

handleGetObject :: String -> W.GameData -> (W.GameData, String)
handleGetObject action state =
    case stringToGameObj $ map toLower (trim action) of
        Just obj -> get obj state
        Nothing -> (state, "I don't recognize that object.")

handleDropObject :: String -> W.GameData -> (W.GameData, String)
handleDropObject action state =
    case stringToGameObj action of
        Just obj -> put obj state
        Nothing -> if action == "coffee mug" then put CoffeeMug state
                   else (state, "You can't drop what you don't have.")


-- The function to display available actions based on the current game state
displayAvailableActions :: W.GameData -> String
displayAvailableActions state =
  let currentRoom = W.getRoomData state
      exitsDescription = "Exits: " ++ unwords (map (\exit -> W.exit_dir exit) (W.exits currentRoom)) ++ ". "
      objectsDescription = "Objects: " ++ unwords (map (\obj -> W.obj_name obj) (W.objects currentRoom)) ++ "."
  in exitsDescription ++ objectsDescription

stringToGameObj :: String -> Maybe GameObj
stringToGameObj name = case map toLower (trim name) of
    "coffee mug" -> Just CoffeeMug
    "full coffee mug" -> Just FullCoffeeMug
    "coffee pot" -> Just CoffeePot
    -- Add more mappings as necessary
    _ -> Nothing

-- Helper function to trim spaces
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- REPL function to interact with the game, showing available actions
repl :: W.GameData -> IO W.GameData
repl state 
    | W.finished state = putStrLn winmessage >> return state
    | otherwise = do
        putStrLn $ show state ++ "\n" ++ displayAvailableActions state
        putStr "What now? "
        hFlush stdout
        input <- getLine
        let (state', msg) = process state input
        putStrLn msg
        repl state'

-- Main function to start the game loop
main :: IO ()
main = do
    _ <- repl W.initState
    return ()