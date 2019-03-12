module PassageDef where

import Data.Set as Set

import GameDef

data PassageType = Unrestricted
                 | Closable Bool -- isClosed
                 | Lockable Bool Bool DefId -- isClosed isLocked keyId
                 deriving (Show, Read)

data PassageDef = PassageDef { defId :: DefId
                             , sDesc :: String
                             , lDesc :: String
                             , keywords :: Set String
                             , passageType :: PassageType
                             } deriving (Show, Read)

                             
instance GameDef PassageDef where
    defId def = PassageDef.defId def
    sDesc def = PassageDef.sDesc def
    lDesc def = PassageDef.lDesc def
    matches def keyword = Set.member keyword $ PassageDef.keywords def


canClose :: PassageType -> Bool
canClose (Closable _ ) = True
canClose (Lockable _ _ _) = True
canClose _ = False

isClosed :: PassageType -> Bool
isClosed (Closable True) = True
isClosed (Lockable True _ _) = True
isClosed _ = False

canLock :: PassageType -> Bool
canLock (Lockable _ _ _) = True
canLock _ = False

isLocked :: PassageType -> Bool
isLocked (Lockable _ True _) = True
isLocked _ = False