{-# LANGUAGE FlexibleContexts #-}

module Actions where

import World
import Data.List (find)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.IO


{-
data GameObj = Mug | CoffeePot | Orb | Dagger | Pill | Door deriving (Eq)

data Direction = North | South | East | West deriving (Eq, Show)

data Imperative = Go Direction | Get GameObj | Drop GameObj | Pour GameObj | Drink GameObj | Open GameObj | Examine GameObj | Swallow GameObj | Lay | Place | Snap | Inventory | Quit deriving (Show, Eq)
-}

actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "examine" = Just examine
actions _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands "lay"       = Just lay
commands "place"     = Just place
commands "snap"      = Just snap
commands "pour"      = Just pour
commands "drink"     = Just drink
commands "swallow"   = Just swallow
commands "open"      = Just open
commands _           = Nothing

{-
parseDirection :: String -> Maybe Direction
parseDirection dir = case str of
      "north" -> Just North
      "south" -> Just South
      "east"  -> Just East
      "west"  -> Just West
      _       -> Nothing

parseGameObj :: String -> Maybe GameObj
parseGameObj obj = case obj of
      "mug"    -> Just Mug
      "coffee" -> Just CoffeePot
      "orb"    -> Just Orb
      "dagger" -> Just Dagger
      "pill"   -> Just Pill
      "door"   -> Just Door
      _        -> Nothing
-}

{-
{-Save Function - Writes GameData to a file-}
save :: GameData -> String -> IO ()
save state file = do
                      BL.writeFile ("./Saves/" ++ file ++ ".save") $ (encode state)
                      putStr ("Saved progress to " ++ file ++ ".save")

{-Load Function - Reads GameData from a file-}
load :: String -> IO GameData
load file = do 
                content <- BL.readFile ("./Saves/" ++ file ++ ".save")
                case decode content of
                   Just x -> return x
                   Nothing -> return initState       
-}

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
move dir rm = find (\x -> x == x) [room x | x <- exits rm, dir == exit_dir x] 
{- Return True if the object appears in the room. -}

objectHere :: String -> Room -> Bool
objectHere o rm = elem o [obj_name x | x <- objects rm, o == obj_name x]

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = rm { objects = newObjs } 
   where newObjs = filter (\x -> obj_name x /= o) (objects rm)

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm { objects = o:(objects rm) }

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj o ds = head [x | x <- ds, o == obj_name x]

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object
objectData o rm = findObj o (objects rm)

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = gd { location_id = rmid, world = newWorld }
   where newWorld | elem (rmid, rmdata) (world gd) = (world gd)
                  | otherwise = (rmid, rmdata):(world gd)

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> String -> GameData
addInv gd obj = gd { inventory = updatedInv }
   where updatedInv = foundObj : (inventory gd)
         foundObj = objectData obj (World.getRoomData gd)

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj = gd { inventory = updatedInv }
   where updatedInv = [x | x <- inventory gd, obj /= obj_name x]

{- Does the inventory in the game state contain the given object? -}
carrying :: GameData -> String -> Bool
carrying gd obj = elem obj [obj_name x | x <- inventory gd, obj == obj_name x]

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state = case move dir (World.getRoomData state) of
      Just x -> ((state { location_id = x }), "You go " ++ dir)
      Nothing -> (state, "You can't go that way.")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'oom_desc (World.getRoomData state)
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state = case objectHere obj (World.getRoomData state) of
      True -> (newState { inventory = (objectData obj (World.getRoomData state)):(inventory state)}, "You get the " ++ obj)
         where newState = updateRoom state (location_id state) (removeObject obj (World.getRoomData state))
      False -> (state, "There is no " ++ obj ++ " in this room.")

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.[x | x <- inventory state, obj == obj_name x]
-}

put :: Action
put obj state = case carrying state obj of
      True -> (newState {location_id = location_id newState, inventory = inventory (removeInv newState obj)}, "You put the " ++ obj ++ " down.")
         where newState = updateRoom state (location_id state) (addObject newObj (World.getRoomData state))
               newObj = findObj obj $ inventory state
      False -> (state, "You are not holding the " ++ obj ++ ".")
      
{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state | carrying state obj = (state, obj_desc (head [x | x <- inventory state, obj == obj_name x]))
                  | objectHere obj (World.getRoomData state) = (state, obj_desc (objectData obj (World.getRoomData state)))
                  | otherwise = (state, "That object is not in the room or your inventory.")

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Command
pour state = case carrying state "mug" && carrying state "coffee" of
      True -> ( state { inventory = (fullmug):(inventory (removeInv state "mug")), poured = True}, "You pour the coffee into the mug!")
      False -> (state, "You don't have both the coffee and the mug. Use command pour.")

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Command
drink state =  case carrying state "mug" && poured state of
      True -> ( state { inventory = (mug):(inventory (removeInv state "mug")), caffeinated = True}, "You drink the coffee!")
      False -> (state, "You don't have a full mug of coffee. Use command drink.")

{- Removes headache state allows player to go to lectures -}

swallow :: Command
swallow state = case carrying state "pill" && not (medicated state) of
      True -> ( state { inventory = (inventory (removeInv state "pill")), medicated = True}, "You take the paracetamol! What sweet relief!")
      False -> (state, "You don't have a pill. ")

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Command
open state = case caffeinated state && (World.getRoomData state) == hall && medicated state of
      True -> ( newState {location_id = "openHall"}, "You open the door!")
            where newState = updateRoom state "openHall" (Room openedhall openedexits [])
      False -> (state, "You haven't drank your coffee and taken your medicine.")

{- Places the orb in the shrine statue. Updates the room with the dagger. -}

place :: Command
place state = case carrying state "orb" && (World.getRoomData state) == shrine of
      True -> (newState { inventory = inventory (removeInv state "orb") }, "You place the orb in the statue. It's pyrite fingers enclosing it violently. \nOut of the base of the statue spits a small dagger.")
         where newState = updateRoom state "shrine" (wokenShrine)
      False -> (state, "Place the orb in the shrine room.")

{- Makes the player lay down in the altar. -}

lay :: Command
lay state = case carrying state "dagger" && (location_id state) == "altar" of
      True -> ( state {location_id = "street"}, "You lay down into the cutout in the altar. Your hand gripping the dagger fervently. \nYou don't understand why you'd feel compelled to do this, but you need to get to lectures - Ian Gent won't accept disappearance. \nYou squeeze your eyes tightly shut and grimace, grasping your left palm around the blade, wincing at the piercing wave of pain. \n'Your wish is granted.' you hear from a 'voice' ringing in your skull. \nWith a flash, you find yourself outside of your house. Cool.\n")
      False -> ( state, "You lay down for a while. It's weirdly comfortable but you find that nothing happens. Maybe you need a tool of some sort?")

{- Makes the player take a photo -}

snap :: Command
snap state = case (location_id state) == "bedroom" of
      True -> (state, "Pulling out your phone, you take a picture of yourself in the mirror. \n'Look at you, you greek sculpture' you think, 'DaVinci only dreamed of such a perfect human form.' \n'Let me take a few more, the suitors will love it.' \nUnfortunately, you remember that you are a CompSci student, and these 'suitors' don't exist. \nDamn.")
      False -> (state, "Your mirror is in the bedroom.")

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")


prop_gokitchen :: Bool
prop_gokitchen = (go "north" (initState)) ==  ((initState {location_id = "kitchen"}), "You go north")

prop_gonowhere :: String -> Bool
prop_gonowhere dir = go dir (initState) ==  (initState, "You can't go that way.")

prop_getmug :: Bool
prop_getmug = (get "mug" (initState)) ==  (newState { inventory = (objectData "mug" (getRoomData initState)):(inventory initState)}, "You get the mug") 
                                     where newState = updateRoom initState (location_id initState) (removeObject "mug" (getRoomData initState))
                             
prop_getnothing :: String -> Bool
prop_getnothing obj = get obj (initState) ==  (initState, "There is no " ++ obj ++ " in this room.")

--examine the mug in the room
prop_examineroom :: Bool
prop_examineroom = examine "mug" (initState) ==  (initState, obj_desc mug)

--examine the mug in the inventory
prop_examineinv :: Bool
prop_examineinv = (examine "mug" (addInv initState "mug")) == ((addInv initState "mug"), obj_desc mug)

prop_examineNothing :: String -> Bool
prop_examineNothing obj = examine obj (initState) ==  (initState, "That object is not in the room or your inventory.")

prop_pour :: Bool
prop_pour = pour state == (state { inventory = (fullmug):(inventory (removeInv state "mug")), poured = True}, "You pour the coffee into the mug!")
                                 where state = initState {inventory = [mug, coffeepot]}
--pour with coffee pot but no mug
prop_pourNoMug :: Bool
prop_pourNoMug = pour state == (state, "You don't have both the coffee and the mug. Use command pour.")
                                 where state = initState {inventory = [coffeepot]}
--pour with mug but no coffee pot
prop_pourNoCoffee :: Bool
prop_pourNoCoffee = pour state == (state, "You don't have both the coffee and the mug. Use command pour.")
                                 where state = initState {inventory = [mug]}
--pour with empty inventory
prop_pourNothing :: Bool
prop_pourNothing = pour initState == (initState, "You don't have both the coffee and the mug. Use command pour.")

--drink with a full mug
prop_drink :: Bool
prop_drink = drink state == (state { inventory = (mug):(inventory (removeInv state "mug")), caffeinated = True}, "You drink the coffee!")
                                 where state = initState {inventory = [mug], poured = True}
--drink with an empty mug
prop_drinkEmpty :: Bool
prop_drinkEmpty = drink state == (state, "You don't have a full mug of coffee. Use command drink.")
                                 where state = initState {inventory = [mug]}

--drink with a poured mug but not in inventory
prop_drinkNoMug :: Bool
prop_drinkNoMug = drink state == (state, "You don't have a full mug of coffee. Use command drink.")
                                 where state = initState {poured = True }
--drink with empty inventory
prop_drinkNothing :: Bool
prop_drinkNothing = drink state == (state, "You don't have a full mug of coffee. Use command drink.")
                                 where state = initState
--swallow with pill
prop_swallow :: Bool
prop_swallow = swallow state == ( state { inventory = (inventory (removeInv state "pill")), medicated = True}, "You take the paracetamol! What sweet relief!")
                                 where state = initState {inventory = [pill]}
--swallow with empty inventory
prop_swallowNothing :: Bool
prop_swallowNothing = swallow state == (state, "You don't have a pill. ")
                                 where state = initState
--open door
prop_open :: Bool
prop_open = open state == ( newState {location_id = "openHall"}, "You open the door!")
                           where newState = updateRoom state "openHall" (Room openedhall openedexits [])
                                 state = initState {location_id = "hall", caffeinated = True, medicated = True}
--open door without being caffeinated                                 
prop_openUncaffinated :: Bool
prop_openUncaffinated = open state == (state, "You haven't drank your coffee and taken your medicine.")
                           where state = initState {location_id = "hall", medicated = True}
--open door without being medicated
prop_openUnmedicated :: Bool
prop_openUnmedicated = open state == (state, "You haven't drank your coffee and taken your medicine.")
                           where state = initState {location_id = "hall", caffeinated = True}
--open door without being medicated or caffeinated
prop_openNotReady :: Bool
prop_openNotReady = open state == (state, "You haven't drank your coffee and taken your medicine.")
                           where state = initState {location_id = "hall"}
--open the front door not in the hall
prop_openWrongLocation :: Bool
prop_openWrongLocation = open state == (state, "You haven't drank your coffee and taken your medicine.")
                           where state = initState

prop_lay :: Bool
prop_lay = lay state == ( state {location_id = "street"}, "You lay down into the cutout in the altar. Your hand gripping the dagger fervently. \nYou don't understand why you'd feel compelled to do this, but you need to get to lectures - Ian Gent won't accept disappearance. \nYou squeeze your eyes tightly shut and grimace, grasping your left palm around the blade, wincing at the piercing wave of pain. \n'Your wish is granted.' you hear from a 'voice' ringing in your skull. \nWith a flash, you find yourself outside of your house. Cool.\n")
                           where state = initState {location_id = "altar", inventory = [dagger]}

prop_layWrongLocation :: Bool
prop_layWrongLocation = lay state == ( state, "You lay down for a while. It's weirdly comfortable but you find that nothing happens. Maybe you need a tool of some sort?")
                           where state = initState {inventory = [dagger]}

prop_layNoDagger :: Bool
prop_layNoDagger = lay state == ( state, "You lay down for a while. It's weirdly comfortable but you find that nothing happens. Maybe you need a tool of some sort?")
                           where state = initState {location_id = "altar"}

prop_snap :: Bool
prop_snap = snap state == (state, "Pulling out your phone, you take a picture of yourself in the mirror. \n'Look at you, you greek sculpture' you think, 'DaVinci only dreamed of such a perfect human form.' \n'Let me take a few more, the suitors will love it.' \nUnfortunately, you remember that you are a CompSci student, and these 'suitors' don't exist. \nDamn.")
                           where state = initState

prop_snapWrongLocation :: Bool
prop_snapWrongLocation = snap state == (state, "Your mirror is in the bedroom.")
                           where state = initState {location_id = "kitchen"}

--inv with empty inventory
prop_inv :: Bool
prop_inv = inv initState == (initState, "You aren't carrying anything")

--inv with mug
prop_invWithMug :: Bool
prop_invWithMug = inv state == (state, "You are carrying:\na coffee mug")
               where state = initState {inventory = [mug]}

--inv with mug
prop_invWithMoreItems :: Bool
prop_invWithMoreItems = inv state == (state, "You are carrying:\na coffee mug\na pot of coffee\nan ashen ritual dagger")
               where state = initState {inventory = [mug, coffeepot, dagger]}