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
-- Moves the player in the specified direction if possible.
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
-- Allows the player to pick up an object if it's in the current room.
get :: GameObj -> W.GameData -> (W.GameData, String)
get obj state =
  let currentRoom = W.getRoomData state
      objectName = show obj
  in if W.objectHere objectName currentRoom
     then let updatedRoom = W.removeObject objectName currentRoom
              updatedState = W.addInv state (W.objectData objectName currentRoom)
              newWorld = W.updateRoomInWorld (W.location_id state) updatedRoom (W.world state)
          in (updatedState { W.world = newWorld }, objectName ++ " taken.")
     else (state, "I don't see a " ++ objectName ++ " here.")

-- Implement the 'put' (drop) action.
-- Allows the player to drop an object they are carrying.
put :: GameObj -> W.GameData -> (W.GameData, String)
put obj state =
  let currentRoom = W.getRoomData state
      objectName = show obj
  in if W.carrying state objectName
     then let updatedRoom = W.addObject (W.objectData objectName currentRoom) currentRoom
              updatedState = W.removeInv state objectName
              newWorld = W.updateRoomInWorld (W.location_id state) updatedRoom (W.world state)
          in (updatedState { W.world = newWorld }, "You drop the " ++ objectName ++ ".")
     else (state, "You are not carrying a " ++ objectName ++ ".")

-- Implement the 'pour' action.
-- Handles the logic for pouring coffee into the mug.
pour :: GameObj -> W.GameData -> (W.GameData, String)
pour Coffee state =
  if W.carrying state "Coffee" && W.carrying state "Mug"
  then (state { W.poured = True }, "You pour the coffee into the mug.")
  else (state, "You can't pour coffee here.")

-- Implement the 'drink' action.
-- Handles the logic for drinking coffee from the mug.
drink :: GameObj -> W.GameData -> (W.GameData, String)
drink Mug state =
  if W.poured state
  then (state { W.caffeinated = True }, "You drink the coffee. You feel energized!")
  else (state, "There is nothing in the mug to drink.")

-- Implement the 'open' action.
-- Handles the logic for opening the door.
open :: GameObj -> W.GameData -> (W.GameData, String)
open Door state =
  if W.caffeinated state
  then (state { W.finished = True }, "You open the door and step outside.")
  else (state, "The door is locked. You need some coffee to get going.")

-- Implement the 'examine' action.
-- Allows the player to examine an object.
examine :: GameObj -> W.GameData -> (W.GameData, String)
examine obj state =
  let currentRoom = W.getRoomData state
      objectName = show obj
  in if W.objectHere objectName currentRoom || W.carrying state objectName
     then (state, W.objectDesc (W.objectData objectName currentRoom))
     else (state, "I don't see a " ++ objectName ++ " here.")

-- Implement the 'inventory' command.
-- Lists the objects the player is currently carrying.
inv :: W.GameData -> (W.GameData, String)
inv state =
  let inventoryList = map W.obj_longname (W.inventory state)
  in (state, "You are carrying: " ++ unwords inventoryList)

-- Implement the 'quit' command.
-- Sets the game state to finished.
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
