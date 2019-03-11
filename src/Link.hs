module Link where

import LinkDef
import GameDef
import GameObj

data Link = Link { def :: LinkDef }

instance GameObj Link where
    sDesc link = GameDef.sDesc $ Link.def link
    lDesc link = GameDef.lDesc $ Link.def link
    matches link keyword = GameDef.matches (Link.def link) keyword