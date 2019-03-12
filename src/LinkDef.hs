module LinkDef where

import Data.Set as Set
import Data.Char (toLower)

import GameDef
import GameObj

data Direction = North | South | East | West | Up | Down | Enter String (Set String) deriving (Show, Read)

data LinkDef = LinkDef { direction :: Direction
                       , targetRoomId :: DefId
                       , passageId :: Maybe DefId
                       } deriving (Show, Read)

getLabel :: Direction -> String
getLabel North = "North"
getLabel South = "South"
getLabel East  = "East"
getLabel West  = "West"
getLabel Up    = "Up"
getLabel Down  = "Down"
getLabel (Enter _ _) = error "Should never call getLabel on 'Enter' direction"

getKeywords :: Direction -> Set String
getKeywords (Enter _ keywords) = keywords
getKeywords dir = Set.singleton (fmap toLower $ getLabel dir)

instance GameDef LinkDef where
    defId def = (-1, -1)
    sDesc def = case LinkDef.direction def of
        Up -> "an opening in the ceiling"
        Down -> "an opening in the floor"
        Enter sDesc _ -> sDesc
        dir -> "a passage to the " ++ (getLabel dir)
    lDesc def = "A passage " ++ GameDef.sDesc def
    matches def keyword = Set.member keyword $ getKeywords $ direction def