module Main where

import Rogue.Interface
import Rogue.Types
import Rogue.World
import Rogue.Actions

import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as M


main :: IO ()
main = runRogue rogue demoConf demoState

demoConf :: RConfig
demoConf = RConfig { 
      worldSize = demoWorldSize
    , screenSize = (80, 22)
    , worldGlyphs = demoGlyphs
    , bindings = demoBindings
    }

demoState :: RState
demoState = RState { 
      world = blankWorld demoWorldSize
    , enemies = M.empty
    , player = demoChar
    , done = False
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
    , position = (0,0)
    , name = "player"
    , glyph = '@'
    }

demoGlyphs :: WorldGlyphMap
demoGlyphs = M.fromList [
      (Floor, '.')
    , (Wall, '#')
    ]

