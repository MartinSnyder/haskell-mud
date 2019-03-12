module CommandHelpers where

import LinkDef
import GameObj
import Room
import Mob
import Link
import Passage
import World

formatContents :: String -> String -> (a -> String) -> [a] -> String
formatContents empty label fmt contents = case contents of
    [] -> empty
    first : rest -> foldl (\acc s -> acc ++ ", " ++ s) (label ++ fmt first) (fmap fmt $ rest)

combine :: [String] -> String
combine strings = case strings of
    [] -> ""
    first : rest -> foldl (\acc el -> acc ++ "\n" ++ el) first $ filter (/= "") rest

lookRoomShort :: Room -> [Mob] -> String
lookRoomShort room roomMobs =
    let
        components = [ GameObj.sDesc room
                        , formatContents "" "Occupants: " GameObj.sDesc roomMobs
                        , formatContents "" "Items:     " GameObj.sDesc $ Room.items room
                        ]
    in combine components

lookRoomLong :: Room -> [Mob] -> String
lookRoomLong room roomMobs =
    let
        components = [ GameObj.sDesc room
                        , GameObj.lDesc room
                        , formatContents "" "Exits:     " exitString $ Room.links room
                        , formatContents "" "Occupants: " GameObj.sDesc roomMobs
                        , formatContents "" "Items:     " GameObj.sDesc $ Room.items room
                        ]
    in combine components

testPassage :: Link -> World -> (Passage -> Bool) -> Either String Bool
testPassage link world predicate =
    case (LinkDef.passageId $ Link.def link) of
        Nothing -> Right False
        Just passageId -> do
            passage <- getPassage passageId world
            return $ predicate passage

updateLink :: (Passage -> Either String Passage) -> Link -> World -> Either String World
updateLink f link world =
    case (LinkDef.passageId $ Link.def link) of
        Nothing -> Left $ "There is no passage associated with this link"
        Just passageId -> updatePassage f passageId world
    