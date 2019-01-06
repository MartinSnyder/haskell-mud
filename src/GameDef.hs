module GameDef where

type DefId = (Integer, Integer)

class GameDef a where
    defId :: a -> DefId
    sDesc :: a -> String
    lDesc :: a -> String