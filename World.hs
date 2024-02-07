{-# LANGUAGE DeriveGeneric #-}

module World where

import GHC.Generics 
import Data.Aeson (FromJSON, ToJSON)

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving (Eq, Generic)
instance ToJSON Object
instance FromJSON Object

instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: String,
                   exit_desc :: String,
                   room :: String }
   deriving (Eq, Generic)
instance ToJSON Exit
instance FromJSON Exit

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving (Eq, Generic)
instance ToJSON Room 
instance FromJSON Room

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           medicated :: Bool, -- pill taken
                           finished :: Bool -- set to True at the end
                         }
   deriving Generic
instance ToJSON GameData 
instance FromJSON GameData


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

mug, fullmug, coffeepot, orb, dagger, pill :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
orb       = Obj "orb" "a peculiar fleshy crimson orb" "A dark crimson orb that seems to be made of organic matter. \nIt seems to perfectly fit into something...?"
dagger    = Obj "dagger" "an ashen ritual dagger" "A patterned obsidian ritual dagger, embossed with a shard of stained sapphire. \nHolding it makes you feel faint."
pill      = Obj "pill" "a paracetamol pill" "A singular 500mg pill of paracetamol."

bedroom, kitchen, hall, street, altar, shrine, fun, wokenShrine :: Room

bedroom = Room "You are in your bedroom. You consider but decide against getting back in bed. \nYou glance at your mirror. \n('snap')\n"
               [Exit "north" "To the north is a kitchen. " "kitchen",
                Exit "south" "To the south is the fun room. " "fun"]
               [mug]

fun = Room "You are in the fun room. \nThere is a single wooden chair facing a blank white wall. \nYou reminisce about countless riveting hours spent here."
           [Exit "north" "To the north is your bedroom. " "bedroom"]
           [pill]

kitchen = Room "You are in the kitchen."
               [Exit "south" "To the south is your bedroom. " "bedroom",
                Exit "west" "To the west is a hallway. " "hall",
                Exit "east" "To the east is the altar room." "altar"]
               [coffeepot]

altar = Room "You are in the altar room. You forget why or when you added this to the house. \nThere is a human shaped cutout in the cobbled pedestal of the altar, for some reason - exactly your size. \n('lay') \n\nThe door has disappeared behind you."
             [Exit "east" "To the east is an ominous arched tunnel snaking downwards. " "shrine"]
             [orb]

shrine = Room "At the end of the tunnel you reach a shrine to... you can't imagine what. \nA blackened marble sculpture depicts a frightening amalgam of hideous warped flesh. \nOne of its many 'arms' is outstretched, reaching for something... \n('place')\n"
              [Exit "west" "To the west is the tunnel you entered from. " "altar"]
              []

wokenShrine = Room "A blackened marble sculpture depicts a frightening amalgam of hideous warped flesh. \nIt's leering visage now reveals an impish sneer. With its hands closed around the orb - you hope it is sated."
                   [Exit "west" "To the west is the tunnel you entered from. " "altar"]
                   [dagger]

hall = Room "You are in the hallway. The front door is closed. ('open') "
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
             ("street", street),
             ("altar", altar),
             ("shrine", shrine),
             ("fun", fun),
             ("wokenShrine", wokenShrine)]

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
