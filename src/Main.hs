module Main where

import Rogue.Types
import Rogue.Actions
import Rogue.Interface
import Rogue.World
import Rogue.WorldGen
import Rogue.Util

import Data.Array (array, (!))
import System.Random (newStdGen, mkStdGen)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens hiding (Level)
import Graphics.Vty

main :: IO ()
main = do
    vty <- mkVty
    g <- newStdGen 

    let (w,g') = randomWorld g
        demoConf = RConfig 
            { _glyphs = demoGlyphs
            , _bindings = demoBindings
            , _viewRadius = 11
            , _term = vty
            , _numLevels = 3
            }

        demoState = RState 
            { _depth = 1
            , _player = demoChar
            , _exitGameNow = False
            , _stdGenR = g'
            , _currentLevel = emptyLevel & world .~ w
            , _upperLevels = []
            , _lowerLevels = []
            }

    runRogue rogue demoConf demoState
    shutdown vty

demoBindings :: Bindings
demoBindings = M.fromList
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
    , (EvKey (KASCII 's') [], getStairsInfo)
    , (EvKey (KASCII 't') [], positionPlayerRandomly)
    , (EvKey KEsc [], quit)
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
