module World where

import Data.List -- Importing List module for list manipulation functions.

-- Data structure for objects in the game.
data Object = Obj { obj_name :: String,      -- Name of the object.
                    obj_longname :: String,  -- Long (descriptive) name of the object.
                    obj_desc :: String }     -- Description of the object.
   deriving Eq -- Making Object instances comparable for equality.

-- Custom show instance for objects, to display their long names.
instance Show Object where
   show obj = obj_longname obj

-- Data structure for exits from rooms.
data Exit = Exit { exit_dir :: String,       -- Direction of the exit.
                   exit_desc :: String,      -- Description of the exit.
                   room :: String }          -- Room that this exit leads to.
   deriving Eq

-- Data structure for rooms in the game.
data Room = Room { room_desc :: String,      -- Description of the room.
                   exits :: [Exit],          -- Exits from the room.
                   objects :: [Object] }     -- Objects in the room.
   deriving Eq

-- Data structure for game data/state.
data GameData = GameData { location_id :: String,   -- Current location ID.
                           world :: [(String, Room)], -- Mapping of room IDs to rooms.
                           inventory :: [Object],     -- Player's inventory.
                           poured :: Bool,            -- Status flag (e.g., whether coffee has been poured).
                           caffeinated :: Bool,       -- Status flag (e.g., whether player is caffeinated).
                           finished :: Bool }         -- Status flag to indicate if the game is finished.
   deriving Eq

-- Function to get the description of an object.
objectDesc :: Object -> String
objectDesc obj = obj_desc obj

-- Function to update a room in the game world.
updateRoomInWorld :: String -> Room -> [(String, Room)] -> [(String, Room)]
updateRoomInWorld roomId updatedRoom = map updateRoom
  where
    updateRoom (rId, room) 
      | rId == roomId = (roomId, updatedRoom) -- Update the specified room.
      | otherwise     = (rId, room)           -- Keep other rooms as they are.

-- Function to find an object's data in a room.
objectData :: String -> Room -> Object
objectData objName room = 
    case find (\obj -> obj_name obj == objName) (objects room) of
        Just obj -> obj
        Nothing -> error $ "Object " ++ objName ++ " not found in room."

-- Function to check if the game has been won.
won :: GameData -> Bool
won gd = location_id gd == "street"

-- Custom show instance for rooms.
instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs

-- Custom show instance for game data.
instance Show GameData where
    show gd = show (getRoomData gd)

-- Type alias for actions that modify an object and update the game state.
type Action  = String -> GameData -> (GameData, String)

-- Type alias for commands that just update the game state.
type Command = GameData -> (GameData, String)

-- Predefined objects in the game.
mug, fullmug, coffeepot :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"

-- Predefined rooms in the game.
bedroom, kitchen, hall, street :: Room
bedroom = Room "You are in your bedroom."
               [Exit "north" "To the north is a kitchen. " "kitchen"]
               [mug]

kitchen = Room "You are in the kitchen."
               [Exit "south" "To the south is your bedroom. " "bedroom",
                Exit "west" "To the west is a hallway. " "hall"]
               [coffeepot]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit "east" "To the east is a kitchen. " "kitchen"]
            []

-- New data about the hall when the door is opened.
openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "out" "You can go outside. " "street"]

street = Room "You have made it out of the house."
              [Exit "in" "You can go back inside if you like. " "hall"]
              []

-- World data: mapping of room names to room data.
gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street)]

-- Initial game state.
initState :: GameData
initState = GameData "bedroom" gameworld [] False False False

-- Function to get room data based on current game state.
-- works using lookup function and checking location id we are storing in world
getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))

-- New helper functions
-- Function to check if an object is in a room.
objectHere :: String -> Room -> Bool
objectHere objName room = any (\obj -> obj_name obj == objName) (objects room)

-- Function to remove an object from a room.
removeObject :: String -> Room -> Room
removeObject objName room = room { objects = filter (\obj -> obj_name obj /= objName) (objects room) }

-- Function to add an object to a room.
addObject :: Object -> Room -> Room
addObject obj room = room { objects = obj : objects room }

-- Function to find an object in a list of objects.
findObject :: String -> [Object] -> Maybe Object
findObject objName objs = find (\obj -> obj_name obj == objName) objs

-- Function to check if the player is carrying an object.
carrying :: GameData -> String -> Bool
carrying gd objName = any (\obj -> obj_name obj == objName) (inventory gd)

-- Function to add an object to the player's inventory.
addInv :: GameData -> Object -> GameData
addInv gd obj = gd { inventory = obj : inventory gd }

-- Function to remove an object from the player's inventory.
removeInv :: GameData -> String -> GameData
removeInv gd objName = gd { inventory = filter (\obj -> obj_name obj /= objName) (inventory gd) }
