module Actions where

import Data.List (find)
import qualified World as W

-- Enumeration of game objects.
data GameObj = Coffee | Mug | Door | Key deriving (Eq, Show)
-- Enumeration of directions.
data Direction = North | South | East | West deriving (Eq, Show)
-- Enumeration of possible commands in the game.
data Command = Go Direction | Get GameObj | Drop GameObj | Pour GameObj | Drink GameObj | Open GameObj | Examine GameObj | Inventory | Quit deriving (Eq, Show)

-- Type alias for actions that involve a game object and update the game state.
type Action  = GameObj -> W.GameData -> (W.GameData, String)

-- Implement the 'go' action.
go :: Direction -> W.GameData -> (W.GameData, String)
go dir state = case dir of
    North -> move "north" state
    South -> move "south" state
    East  -> move "east" state
    West  -> move "west" state
  where
    move direction state = case find ((== direction) . W.exit_dir) (W.exits (W.getRoomData state)) of
        Just exit -> (state { W.location_id = W.room exit }, "You go " ++ direction)
        Nothing -> (state, "You can't go that way.")

-- Implement the 'get' action.
get :: GameObj -> W.GameData -> (W.GameData, String)
get obj state =
  let currentRoom = W.getRoomData state
      objectName = show obj
  in if objectHere objectName currentRoom
     then let updatedRoom = removeObject objectName currentRoom
              updatedState = addInv state (W.objectData objectName currentRoom)
              newWorld = W.updateRoomInWorld (W.location_id state) updatedRoom (W.world state)
          in (updatedState { W.world = newWorld }, objectName ++ " taken.")
     else (state, "I don't see a " ++ objectName ++ " here.")

-- Implement the 'put' (drop) action.
put :: GameObj -> W.GameData -> (W.GameData, String)
put obj state =
  let currentRoom = W.getRoomData state
      objectName = show obj
  in if carrying state objectName
     then let updatedRoom = addObject (W.objectData objectName currentRoom) currentRoom
              updatedState = removeInv state objectName
              newWorld = W.updateRoomInWorld (W.location_id state) updatedRoom (W.world state)
          in (updatedState { W.world = newWorld }, "You drop the " ++ objectName ++ ".")
     else (state, "You are not carrying a " ++ objectName ++ ".")

-- Implement the 'pour' action.
pour :: GameObj -> W.GameData -> (W.GameData, String)
pour Coffee state =
  if carrying state "Coffee" && carrying state "Mug"
  then (state { W.poured = True }, "You pour the coffee into the mug.")
  else (state, "You can't pour coffee here.")

-- Implement the 'drink' action.
drink :: GameObj -> W.GameData -> (W.GameData, String)
drink Mug state =
  if W.poured state
  then (state { W.caffeinated = True }, "You drink the coffee. You feel energized!")
  else (state, "There is nothing in the mug to drink.")

-- Implement the 'open' action.
open :: GameObj -> W.GameData -> (W.GameData, String)
open Door state =
  if W.caffeinated state
  then (state { W.finished = True }, "You open the door and step outside.")
  else (state, "The door is locked. You need some coffee to get going.")

-- Implement the 'examine' action.
examine :: GameObj -> W.GameData -> (W.GameData, String)
examine obj state =
  let currentRoom = W.getRoomData state
      objectName = show obj
  in if objectHere objectName currentRoom || carrying state objectName
     then (state, W.objectDesc (W.objectData objectName currentRoom))
     else (state, "I don't see a " ++ objectName ++ " here.")

-- Implement the 'inventory' command.
inv :: W.GameData -> (W.GameData, String)
inv state =
  let inventoryList = map W.obj_longname (W.inventory state)
  in (state, "You are carrying: " ++ unwords inventoryList)

-- Implement the 'quit' command.
quit :: W.GameData -> (W.GameData, String)
quit state = (state { W.finished = True }, "Quitting the game.")

-- Map string commands to their respective action functions.
actions :: String -> Maybe (GameObj -> W.GameData -> (W.GameData, String))
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "drink"   = Just drink
actions "open"    = Just open
actions "examine" = Just examine
actions _         = Nothing

-- Map string commands to their respective game command functions.
commands :: String -> Maybe (W.GameData -> (W.GameData, String))
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

-- Moved helper functions from World.hs

-- Function to check if an object is in a room.
objectHere :: String -> W.Room -> Bool
objectHere objName room = any (\obj -> W.obj_name obj == objName) (W.objects room)

-- Function to remove an object from a room.
removeObject :: String -> W.Room -> W.Room
removeObject objName room = room { W.objects = filter (\obj -> W.obj_name obj /= objName) (W.objects room) }

-- Function to add an object to a room.
addObject :: W.Object -> W.Room -> W.Room
addObject obj room = room { W.objects = obj : W.objects room }

-- Function to find an object in a list of objects.
findObject :: String -> [W.Object] -> Maybe W.Object
findObject objName objs = find (\obj -> W.obj_name obj == objName) objs

-- Function to check if the player is carrying an object.
carrying :: W.GameData -> String -> Bool
carrying gd objName = any (\obj -> W.obj_name obj == objName) (W.inventory gd)

-- Function to add an object to the player's inventory.
addInv :: W.GameData -> W.Object -> W.GameData
addInv gd obj = gd { W.inventory = obj : W.inventory gd }

-- Function to remove an object from the player's inventory.
removeInv :: W.GameData -> String -> W.GameData
removeInv gd objName = gd { W.inventory = filter (\obj -> W.obj_name obj /= objName) (W.inventory gd) }
