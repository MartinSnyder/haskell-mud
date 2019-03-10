module LinkDef where

import Data.Char (toLower)

import GameDef
import GameObj

data LinkDef = LinkDef { name :: String
                       , targetRoomId :: DefId
                       } deriving (Show, Read, Eq)

instance GameDef LinkDef where
    defId def = (-1, -1)
    sDesc def = LinkDef.name def
    lDesc def = LinkDef.name def
    matches def keyword = keyword == (map toLower $ LinkDef.name def)