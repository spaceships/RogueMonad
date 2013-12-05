module Main where

import Rogue.Types
import Rogue.Actions
import Rogue.Interface

import Data.Array (array, (!))
import System.Random (newStdGen, mkStdGen)
import System.Console.ANSI
import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    g <- newStdGen 
    runRogue rogue demoConf demoState { _stdGenR = g }
    putStrLn "BYE!"

demoConf :: RConfig
demoConf = RConfig 
    { _screenSize = (80, 40)
    , _glyphs = demoGlyphs
    , _bindings = demoBindings
    , _viewRadius = 11
    }

demoState :: RState
demoState = RState 
    { _world = array ((0,0), demoWorldSize) [((x,y), EmptySpace) | x <- [0..10], y <- [0..10]]
    , _enemies = []
    , _player = demoChar
    , _exitGame = False
    , _stdGenR = mkStdGen 0
    , _seen = S.empty
    , _visible = S.empty
    , _testCircle = S.empty -- TEST
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
    , ('r', genWorld)
    ]

demoChar :: Actor
demoChar = Actor 
    { _hp = 5
    , _maxHp = 10
    , _acc = 10
    , _def = 10 
    , _position = (1,1)
    , _name = "Player"
    }

demoGlyphs :: GlyphMap
demoGlyphs = M.fromList 
    [ ("Floor"     , floorGlyph)
    , ("Wall"      , wallGlyph)
    , ("Player"    , playerGlyph)
    , ("EmptySpace", emptySpaceGlyph)
    ]

emptySpaceGlyph = Glyph 
    { _glyph = ' '
    , _color = [ Reset ]
    }

playerGlyph = Glyph 
    { _glyph = '@'
    , _color = [ SetColor Foreground Vivid White ]
    }

floorGlyph = Glyph 
    { _glyph = '.'
    , _color = [ SetColor Foreground Vivid Black ] 
    } 

wallGlyph = Glyph 
    { _glyph = '#'
    , _color = [ Reset ]
    }
