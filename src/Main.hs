module Main where

import Rogue.Types
import Rogue.Actions
import Rogue.Interface
import Rogue.World
import Rogue.Util

import Data.Array (array, (!))
import System.Random (newStdGen, mkStdGen)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Graphics.Vty

main :: IO ()
main = do
    vty <- mkVty
    g <- newStdGen 

    let w = randomWorld g
        demoConf = RConfig { _glyphs = demoGlyphs
                           , _bindings = demoBindings
                           , _viewRadius = 11
                           , _term = vty
                           }

        demoState = RState { _world = w
                           , _enemies = []
                           , _player = demoChar
                           , _exitGame = False
                           , _stdGenR = g
                           , _seen = S.empty
                           , _visible = S.empty
                           }

    runRogue rogue demoConf demoState
    shutdown vty
    putStrLn "BYE!"

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
    , (EvKey (KASCII 'r') [], genWorld)
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
    ]

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
