module Message (Segment(..), Message, resolveMessage) where

import Data.Maybe
import Data.Char
import Text.Printf

import GameObj (sDesc, lDesc)
import Mob
import Connection

data Segment = Actor
             | ActorPossessive
             | Target
             | TargetPossessive
             | Xtra Bool
             | ActorVerb String String
             | TargetVerb String String
             | Const String
type Message = [Segment]

resolveSegment :: Mob -> Maybe Mob -> String -> Connection -> Segment -> String
resolveSegment _ _ _ _ (Const text) = text
resolveSegment _ _ xtra _ (Xtra quoted) = if quoted then surroundWith "\"" xtra else xtra
resolveSegment actor _ _ recipient Actor = getMobText actor recipient
resolveSegment actor _ _ recipient ActorPossessive = getMobPossessiveText actor recipient
resolveSegment _ possibleTarget _ recipient Target =
    case possibleTarget of
        Just target -> getMobText target recipient
        Nothing -> error "Target specified but not supplied for message"
resolveSegment _ possibleTarget _ recipient TargetPossessive =
    case possibleTarget of
        Just target -> getMobPossessiveText target recipient
        Nothing -> error "Target specified but not supplied for message"
resolveSegment actor _ _ recipient (ActorVerb second third) =
    surroundWith " " $ if isRecipient actor recipient then second else third
resolveSegment _ possiblyTarget _ recipient (TargetVerb second third) =
    surroundWith " " $ if maybeIsRecipient possiblyTarget recipient then second else third

getMobText :: Mob -> Connection -> String
getMobText mob recipient =
    if isRecipient mob recipient then "you" else GameObj.sDesc mob

getMobPossessiveText :: Mob -> Connection -> String
getMobPossessiveText mob recipient =
    if isRecipient mob recipient then "your" else makePossessive $ GameObj.sDesc mob
    where makePossessive s = s ++ "'s"

resolveMessage :: Mob -> Maybe Mob -> String -> Message -> Connection -> String
resolveMessage actor possiblyTarget xtra segments recipient =
    upperFirst $ foldl (\ acc segment -> acc ++ (resolveSegment actor possiblyTarget xtra recipient segment)) "" segments

-- Helper to surround a string with a common character (usually space or quotes)
surroundWith :: String -> String -> String
surroundWith chars string = chars ++ string ++ chars

-- Helper to capitalize the first character of a string
upperFirst :: String -> String
upperFirst "" = ""
upperFirst (c:cs) = toUpper c : cs

maybeIsRecipient :: Maybe Mob -> Connection -> Bool
maybeIsRecipient possiblyMob recipient =
    case possiblyMob of
        Just mob -> isRecipient mob recipient
        Nothing -> False

isRecipient :: Mob -> Connection -> Bool
isRecipient mob recipient = (Mob.id mob) == (Connection.mobId recipient)
