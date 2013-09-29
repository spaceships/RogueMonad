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

demoWorldSize = (30, 10)

demoConf :: RConfig
demoConf = RConfig { 
      worldSize = demoWorldSize
    , screenSize = (80, 22)
    , worldGlyphs = demoGlyphs
    , bindings = demoBindings
    }

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

demoState :: RState
demoState = RState { 
      world = blankWorld demoWorldSize
    , enemies = M.empty
    , player = demoChar
    }

demoChar :: Actor
demoChar = Actor { 
      hp = 5
    , maxHp = 10
    , acc = 10
    , def = 10 
    , pos = (5,5)
    , name = "player"
    , glyph = '@'
    }

demoGlyphs :: WorldGlyphMap
demoGlyphs = M.fromList [
      (Floor, '.')
    , (Wall, '#')
    ]

