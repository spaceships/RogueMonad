module Main where

import Rogue.Interface
import Rogue.Types
import Rogue.World
import Rogue.Actions

import Data.Array
import System.Random
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M

main :: IO ()
main = newStdGen >>= \g -> runRogue rogue demoConf demoState { stdGen = g }

demoConf :: RConfig
demoConf = RConfig { 
      worldSize = demoWorldSize
    , screenSize = (80, 22)
    , maxRooms = 10
    , minRoomSize = (3,3)
    , maxRoomSize = (10,6)
    , worldGlyphs = demoGlyphs
    , bindings = demoBindings
    , tunnelThreshold = 3 / 4
    , overlapAllowed = True
    }

demoState :: RState
demoState = RState { 
      world = array ((0,0), demoWorldSize) [((x,y), Nothing) | x <- [0..10], y <- [0..10]]
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
      (Floor, '.')
    , (Wall, '#')
    ]

