module Message (Source(..), Segment(..), Message, resolveMessage) where

import Data.Maybe
import Data.Char
import Text.Printf
import Data.String.Utils (endswith)

import GameObj
import Mob
import Connection
import Target

data Source = Actor | Target1 | Target2 deriving (Show, Read)

data Segment = Sp
             | Sur String Segment
             | Const String
             | Xtra
             | Desc Source
             | Poss Source
             | Verb Source String String
             | Pro Source
             | PossPro Source
             deriving (Show, Read)

type Message = [Segment]

resolveMessage :: Mob -> Target -> Target -> String -> Message -> Connection -> String
resolveMessage actor target1 target2 xtra segments recipient =
    upperFirst $ foldl (\ acc segment -> acc ++ (resolveSegment actor target1 target2 xtra recipient segment)) "" segments

resolveSegment :: Mob -> Target -> Target -> String -> Connection -> Segment -> String
resolveSegment _ _ _ _ _ Sp =
    " "
resolveSegment actor target1 target2 xtra recipient (Sur token childSegment) =
    surroundWith token (resolveSegment actor target1 target2 xtra recipient childSegment)
resolveSegment _ _ _ _ _ (Const text) =
    text
resolveSegment _ _ _ xtra _ Xtra =
    xtra
resolveSegment actor target1 target2 _ recipient (Desc source) =
    getTargetDescription recipient $ isolateSource source actor target1 target2
resolveSegment actor target1 target2 _ recipient (Poss source) =
    makePossessive $ getTargetDescription recipient $ isolateSource source actor target1 target2
resolveSegment actor target1 target2 _ recipient (Verb source second third) =
    if targetIsRecipient (isolateSource source actor target1 target2) recipient then second else third
resolveSegment actor target1 target2 _ recipient (Pro source) =
    getTargetPronoun recipient $ isolateSource source actor target1 target2
resolveSegment actor target1 target2 _ recipient (PossPro source) =
    getTargetPossessivePronoun recipient $ isolateSource source actor target1 target2

isolateSource :: Source -> Mob -> Target -> Target -> Target
isolateSource source actor target1 target2 = case source of
    Actor -> TargetMob actor
    Target1 -> target1
    Target2 -> target2

getTargetDescription :: Connection -> Target -> String
getTargetDescription recipient target =
    case target of
        TargetLink link -> GameObj.sDesc link
        TargetItem item -> GameObj.sDesc item
        TargetMob mob -> if mobIsRecipient mob recipient then "you" else GameObj.sDesc mob

getTargetPronoun :: Connection -> Target -> String
getTargetPronoun recipient target =
    case target of
        TargetLink link -> "it"
        TargetItem item -> "it"
        TargetMob mob -> if mobIsRecipient mob recipient then "you" else "them"

getTargetPossessivePronoun :: Connection -> Target -> String
getTargetPossessivePronoun recipient target =
    case target of
        TargetLink link -> "its"
        TargetItem item -> "its"
        TargetMob mob -> if mobIsRecipient mob recipient then "your" else "their"

-- Helper to surround a string with a common character (usually space or quotes)
surroundWith :: String -> String -> String
surroundWith token string = token ++ string ++ token

-- Helper to capitalize the first character of a string
upperFirst :: String -> String
upperFirst "" = ""
upperFirst (c:cs) = toUpper c : cs

-- Helper to make a word possessive
makePossessive :: String -> String
makePossessive text =
    if endswith "s" text then text ++ "'" else text ++ "'s"

mobIsRecipient :: Mob -> Connection -> Bool
mobIsRecipient mob recipient =
    (Mob.id mob) == (Connection.mobId recipient)

targetIsRecipient :: Target -> Connection -> Bool
targetIsRecipient target recipient =
    case target of
        TargetNone -> error "Target specified but not supplied for message"
        TargetMob mob -> mobIsRecipient mob recipient
        _ -> False