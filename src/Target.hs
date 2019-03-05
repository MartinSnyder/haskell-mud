module Target where

import Item
import Mob
import Link

data Target = TargetNone
            | TargetItem Item
            | TargetMob Mob
            | TargetLink Link