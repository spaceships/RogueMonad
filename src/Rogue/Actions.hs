module Rogue.Actions where

import Rogue.Types
import Rogue.World

move :: Direction -> Rogue ()
move d = do
    RState world enemies player <- get
    return ()
