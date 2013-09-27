module Main where

import Rogue.Interface
import Rogue.Types

main :: IO ()
main = runRogue play (RConfig (1000, 1000) (80, 23)) (RState [] (Character 5 10 10 10 (5,5) '@'))
