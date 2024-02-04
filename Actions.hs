module Actions where

import World
import Data.List (find)


actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions "place"   = Just place
actions "swallow" = Just swallow
actions _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

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
addObject o rm = rm { objects = newObjs }
   where newObjs = o:(objects rm)

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
updateRoom gd rmid rmdata = gd { location_id = newLocation, world = newWorld }
   where newLocation = rmid
         newWorld | elem (rmid, rmdata) (world gd) = (world gd)
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
   This should only work if the object is in the current room. Use 'objectHere'
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
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state = case carrying state obj of
      True -> (removeInv newState obj, "You put the " ++ obj ++ " down.")
         where newState = updateRoom state (location_id state) (addObject newObj (World.getRoomData state))
               newObj = findObj obj [x | x <- inventory state, obj == obj_name x]
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

pour :: Action
pour obj state = case carrying state "mug" && carrying state "coffee" of
      True -> ( state { inventory = (fullmug):(inventory (removeInv state "mug")), poured = True}, "You pour the coffee into the mug!")
      False -> (state, "You don't have both the coffee and the mug. Use command pour.")

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state =  case carrying state "mug" && carrying state "coffee" && poured state  of
      True -> ( state { inventory = (mug):(inventory (removeInv state "mug")), caffeinated = True}, "You drink the coffee!")
      False -> (state, "You don't have a full mug of coffee. Use command drink.")

{- Removes headache state allows player to go to lectures -}

swallow :: Action
swallow obj state = case carrying state "pill" && not (medicated state)  of
      True -> ( state { inventory = (inventory (removeInv state "pill")), medicated = True}, "You take the paracetamol! What sweet relief!")
      False -> (state, "You don't have a pill. ")

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state = case caffeinated state && (World.getRoomData state) == hall && medicated state of
      True -> ( newState {location_id = "openHall"}, "You open the door!")
            where newState = updateRoom state "openHall" (Room openedhall openedexits [])
      False -> (state, "You haven't drank your coffee and taken your medicine.")

{- Places the orb in the shrine statue. Updates the room with the dagger. -}

place :: Action
place obj state = case carrying state "orb" && (World.getRoomData state) == shrine of
      True -> (newState { inventory = inventory (removeInv state "orb") }, "You place the orb in the statue. It's pyrite fingers enclosing it violently. \nOut of the base of the statue spits a small dagger.")
         where newState = updateRoom state "shrine" (wokenShrine)
      False -> (state, "You don't have the orb on you.")

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

