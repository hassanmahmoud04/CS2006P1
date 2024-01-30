module World where

import Data.List 

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving Eq

instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: String,
                   exit_desc :: String,
                   room :: String }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

data GameData = GameData { location_id :: String,
                           world :: [(String, Room)],
                           inventory :: [Object],
                           poured :: Bool,
                           caffeinated :: Bool,
                           finished :: Bool }
   deriving Eq

-- Function to get the description of an object
objectDesc :: Object -> String
objectDesc obj = obj_desc obj

-- Function to update a room in the world
updateRoomInWorld :: String -> Room -> [(String, Room)] -> [(String, Room)]
updateRoomInWorld roomId updatedRoom = map updateRoom
  where
    updateRoom (rId, room) 
      | rId == roomId = (roomId, updatedRoom)
      | otherwise     = (rId, room)

objectData :: String -> Room -> Object
objectData objName room = 
    case find (\obj -> obj_name obj == objName) (objects room) of
        Just obj -> obj
        Nothing -> error $ "Object " ++ objName ++ " not found in room."

won :: GameData -> Bool
won gd = location_id gd == "street"

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs

instance Show GameData where
    show gd = show (getRoomData gd)

-- Things which do something to an object and update the game state
type Action  = String -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

mug, fullmug, coffeepot :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"

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

-- New data about the hall for when we open the door
openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "out" "You can go outside. " "street"]

street = Room "You have made it out of the house."
              [Exit "in" "You can go back inside if you like. " "hall"]
              []

gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street)]

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))

-- New helper functions
objectHere :: String -> Room -> Bool
objectHere objName room = any (\obj -> obj_name obj == objName) (objects room)

removeObject :: String -> Room -> Room
removeObject objName room = room { objects = filter (\obj -> obj_name obj /= objName) (objects room) }

addObject :: Object -> Room -> Room
addObject obj room = room { objects = obj : objects room }

findObject :: String -> [Object] -> Maybe Object
findObject objName objs = find (\obj -> obj_name obj == objName) objs

carrying :: GameData -> String -> Bool
carrying gd objName = any (\obj -> obj_name obj == objName) (inventory gd)

addInv :: GameData -> Object -> GameData
addInv gd obj = gd { inventory = obj : inventory gd }

removeInv :: GameData -> String -> GameData
removeInv gd objName = gd { inventory = filter (\obj -> obj_name obj /= objName) (inventory gd) }
