module GameObj where

import GameDef

class GameObj a where
    sDesc :: a -> String
    lDesc :: a -> String