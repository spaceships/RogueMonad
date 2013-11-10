module Main where

import Rogue.Types
import Rogue.Actions (move, quit)
import Rogue.Interface (rogue)

import Data.Array (array, (!))
import System.Random (newStdGen, mkStdGen)
import Control.Arrow (second)
import qualified Data.Map as M

main :: IO ()
main = newStdGen >>= \g -> runRogue rogue demoConf demoState { stdGen = g }

demoConf :: RConfig
demoConf = RConfig { 
      worldSize = demoWorldSize
    , screenSize = (80, 22)
    , minRoomSize = (5,3)
    , maxRoomSize = (19,9)
    , worldGlyphs = demoGlyphs
    , bindings = demoBindings
    , tunnelThreshold = 0.75
    , roomOverlapAllowed = False
    , numTunnels = 100
    , onlyTerminalTunnels = True
    }

demoState :: RState
demoState = RState { 
      world = array ((0,0), demoWorldSize) [((x,y), Empty) | x <- [0..10], y <- [0..10]]
    , enemies = M.empty
    , player = demoChar
    , done = False
    , stdGen = mkStdGen 0
    , floors = []
    , walls = []
    }

demoWorldSize = (200, 90)

demoBindings :: Bindings
demoBindings = [
      ('j', move S)
    , ('k', move N)
    , ('h', move W)
    , ('l', move E)
    , ('b', move SW)
    , ('n', move SE)
    , ('y', move NW)
    , ('u', move NE)
    , ('\ESC', quit)
    ]

demoChar :: Actor
demoChar = Actor { 
      hp = 5
    , maxHp = 10
    , acc = 10
    , def = 10 
    , position = (1,1)
    , name = "player"
    , glyph = '@'
    }

demoGlyphs :: WorldGlyphMap
demoGlyphs = M.fromList [
      (Floor, Glyph '.')
    , (Wall, Glyph '#')
    ]
