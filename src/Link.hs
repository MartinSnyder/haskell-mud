module Link where

import LinkDef
import GameObj

data Link = Link { def :: LinkDef
                 } deriving (Show, Eq)