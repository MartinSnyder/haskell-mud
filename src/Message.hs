module Message (Segment(..), Message, resolveMessage) where

import Data.Maybe
import Data.Char
import Text.Printf

import GameObj
import Mob
import Connection
import Target

data Segment = ActorDesc
             | ActorPossessive
             | ActorVerb String String
             | Target1Desc
             | Target1Possessive
             | Target1Verb String String
             | Target2Desc
             | Target2Possessive
             | Target2Verb String String
             | Xtra Bool
             | Const String
type Message = [Segment]

resolveMessage :: Mob -> Target -> Target -> String -> Message -> Connection -> String
resolveMessage actor target1 target2 xtra segments recipient =
    upperFirst $ foldl (\ acc segment -> acc ++ (resolveSegment actor target1 target2 xtra recipient segment)) "" segments

resolveSegment :: Mob -> Target -> Target -> String -> Connection -> Segment -> String
resolveSegment _ _ _ _ _ (Const text) =
    text
resolveSegment _ _ _ xtra _ (Xtra quoted) =
    if quoted then surroundWith "\"" xtra else xtra
resolveSegment actor _ _ _ recipient ActorDesc =
    getMobText actor recipient False
resolveSegment actor _ _ _ recipient ActorPossessive =
    getMobText actor recipient True
resolveSegment actor _ _ _ recipient (ActorVerb second third) =
    surroundWith " " $ if mobIsRecipient actor recipient then second else third
resolveSegment _ target _ _ recipient Target1Desc =
    getTargetText target recipient False
resolveSegment _ target _ _ recipient Target1Possessive =
    getTargetText target recipient True
resolveSegment _ target _ _ recipient (Target1Verb second third) =
    surroundWith " " $ if targetIsRecipient target recipient then second else third
resolveSegment _ _ target _ recipient Target2Desc =
    getTargetText target recipient False
resolveSegment _ _ target _ recipient Target2Possessive =
    getTargetText target recipient True
resolveSegment _ _ target _ recipient (Target2Verb second third) =
    surroundWith " " $ if targetIsRecipient target recipient then second else third
    
getMobText :: Mob -> Connection -> Bool -> String
getMobText mob recipient possessive =
    if mobIsRecipient mob recipient then "you" else GameObj.sDesc mob

getTargetText :: Target -> Connection -> Bool -> String
getTargetText target recipient possessive =
    case target of
        TargetNone -> error "Target specified but not supplied for message"
        TargetLink link -> GameObj.sDesc link
        TargetItem item -> GameObj.sDesc item
        TargetMob mob -> getMobText mob recipient possessive

-- Helper to surround a string with a common character (usually space or quotes)
surroundWith :: String -> String -> String
surroundWith chars string = chars ++ string ++ chars

-- Helper to capitalize the first character of a string
upperFirst :: String -> String
upperFirst "" = ""
upperFirst (c:cs) = toUpper c : cs

mobIsRecipient :: Mob -> Connection -> Bool
mobIsRecipient mob recipient =
    (Mob.id mob) == (Connection.mobId recipient)

targetIsRecipient :: Target -> Connection -> Bool
targetIsRecipient target recipient =
    case target of
        TargetNone -> error "Target specified but not supplied for message"
        TargetMob mob -> mobIsRecipient mob recipient
        _ -> False