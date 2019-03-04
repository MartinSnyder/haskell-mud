module Match where

import Item
import Mob
import Link

data Match = NoMatch
           | MatchItem Item
           | MatchMob Mob
           | MatchLink Link