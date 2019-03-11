module PassageDef where

import Data.Set

import GameDef

data PassageType = Unrestricted
                 | Closable Bool -- isClosed
                 | Lockable Bool Bool DefId -- isClosed isLocked keyId
                 deriving (Show, Read)

data PassageDef = PassageDef { defId :: DefId
                             , lDesc :: String
                             , passageType :: PassageType
                             } deriving (Show, Read)