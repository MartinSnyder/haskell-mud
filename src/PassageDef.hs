module PassageDef where

import Data.Set

import GameDef

data Direction = North | South | East | West | Up | Down | Enter (Set String) deriving (Show, Read)

data LinkDef2 = LinkDef2 { from :: DefId
                         , to :: DefId
                         , direction :: Direction
                         } deriving (Show, Read)

data PassageType = Unrestricted
                 | Closable Bool -- isClosed
                 | Lockable Bool Bool DefId -- isClosed isLocked keyId
                 deriving (Show, Read)

data PassageDef = PassageDef { sDesc :: String
                             , links :: [LinkDef2]
                             , passageType :: PassageType
                             } deriving (Show, Read)