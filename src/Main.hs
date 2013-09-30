module Main where

import Rogue.Interface
import Rogue.Types
import Rogue.World
import Rogue.Actions

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
    , minRooms = 2
    , maxRooms = 10
    , minRoomSize = (2,2)
    , maxRoomSize = (5,5)
    , worldGlyphs = demoGlyphs
    , bindings = demoBindings
    }

demoState :: RState
demoState = RState { 
      world = room demoWorldSize
    , enemies = M.empty
    , player = demoChar
    , done = False
    , stdGen = mkStdGen 0
    }

demoWorldSize = (30, 10)

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

