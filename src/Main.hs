module Main where

import Rogue.Types
import Rogue.Actions (move, quit)
import Rogue.Interface (rogue)

import Data.Array (array, (!))
import System.Random (newStdGen, mkStdGen)
import Control.Lens
import qualified Data.Map as M

main :: IO ()
main = newStdGen >>= \g -> runRogue rogue demoConf demoState { _stdGen = g }

demoConf :: RConfig
demoConf = RConfig 
    { _worldSize = demoWorldSize
    , _screenSize = (80, 22)
    , _minRoomSize = (5,3)
    , _maxRoomSize = (19,9)
    , _worldGlyphs = demoGlyphs
    , _bindings = demoBindings
    , _tunnelThreshold = 0.75
    , _roomOverlapAllowed = False
    , _numTunnels = 100
    , _onlyTerminalTunnels = True
    }

demoState :: RState
demoState = RState 
    { _world = array ((0,0), demoWorldSize) [((x,y), Empty) | x <- [0..10], y <- [0..10]]
    , _enemies = []
    , _player = demoChar
    , _done = False
    , _stdGen = mkStdGen 0
    , _floors = []
    , _walls = []
    }

demoWorldSize = (200, 90)

demoBindings :: Bindings
demoBindings = 
    [ ('j', move S)
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
demoChar = Actor 
    { _hp = 5
    , _maxHp = 10
    , _acc = 10
    , _def = 10 
    , _position = (1,1)
    , _name = "player"
    , _glyph = '@'
    }

demoGlyphs :: WorldGlyphMap
demoGlyphs = M.fromList 
    [ (Floor, Glyph '.')
    , (Wall, Glyph '#')
    ]
