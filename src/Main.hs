module Main where

import Rogue.Types
import Rogue.Actions
import Rogue.Interface
import Rogue.World
import Rogue.WorldGen
import Rogue.Util

import System.Random (newStdGen, mkStdGen)
import Control.Lens ((&), (.~))
import qualified Data.Map as M
import Graphics.Vty

main :: IO ()
main = do
    cfg <- defaultConfig
    st  <- defaultState
    runRogue rogue cfg st

defaultConfig :: IO RConfig
defaultConfig = do
    vty <- mkVty
    return $ RConfig 
        { _glyphs = defaultGlyphs
        , _bindings = defaultBindings
        , _viewRadius = 11
        , _term = vty
        , _numLevels = 3
        , _numStairs = 3
        }

defaultState :: IO RState
defaultState = do
    g <- newStdGen 
    return $ RState 
        { _depth = 0
        , _player = initialPlayer
        , _exitGameNow = False
        , _stdGenR = g
        , _currentLevel = emptyLevel
        , _upperLevels = []
        , _lowerLevels = []
        }

initialPlayer :: Actor
initialPlayer = Actor 
    { _hp = 10
    , _maxHp = 10
    , _acc = 10
    , _def = 10 
    , _position = (1,1)
    , _name = "Player"
    }

defaultBindings :: Bindings
defaultBindings = M.fromList
    [ (EvKey (KASCII 'j') [], move S)
    , (EvKey (KASCII 'k') [], move N)
    , (EvKey (KASCII 'h') [], move W)
    , (EvKey (KASCII 'l') [], move E)
    , (EvKey (KASCII 'b') [], move SW)
    , (EvKey (KASCII 'n') [], move SE)
    , (EvKey (KASCII 'y') [], move NW)
    , (EvKey (KASCII 'u') [], move NE)
    , (EvKey (KASCII '>') [], goDownstairs)
    , (EvKey (KASCII '<') [], goUpstairs)
    , (EvKey (KASCII 'r') [], genNewWorld)
    , (EvKey (KASCII 'i') [], getPosition)
    , (EvKey (KASCII 't') [], positionPlayerRandomly)
    , (EvKey KEsc [], promptExitDungeon)
    ]

defaultGlyphs :: GlyphMap
defaultGlyphs = M.fromList 
    [ ("Floor"       , floorGlyph)
    , ("Wall"        , wallGlyph)
    , ("Player"      , playerGlyph)
    , ("EmptySpace"  , emptySpaceGlyph)
    , ("StairsDown"  , stairsDownGlyph)
    , ("StairsUp"    , stairsUpGlyph)
    ]

stairsDownGlyph = Glyph 
    { _glyph = '>'
    , _color = def_attr `with_fore_color` (Color240 74)
    }

stairsUpGlyph = Glyph 
    { _glyph = '<'
    , _color = def_attr `with_fore_color` (Color240 74)
    }

emptySpaceGlyph = Glyph 
    { _glyph = ' '
    , _color = def_attr `with_fore_color` black
    }

playerGlyph = Glyph 
    { _glyph = '@'
    , _color = Attr (SetTo bold) (SetTo $ bright_white) Default
    }

floorGlyph = Glyph 
    { _glyph = '.'
    , _color = def_attr `with_fore_color` bright_black
    } 

wallGlyph = Glyph 
    { _glyph = '#'
    , _color = def_attr `with_fore_color` (Color240 74)
    }
