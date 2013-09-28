module Main where

import Rogue.Interface
import Rogue.Types
import Rogue.World
import Rogue.Actions
import qualified Data.Map as Map

main :: IO ()
main = runRogue play demoConf demoState

demoWorldSize = (100, 100)

demoConf :: RConfig
demoConf = RConfig { 
                     worldSize = demoWorldSize
                   , screenSize = (80, 22)
                   , worldGlyphs = demoGlyphs
                   }

demoState :: RState
demoState = RState { 
                     world = blankWorld demoWorldSize
                   , enemies = []
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
demoGlyphs = Map.fromList [
                            (Floor, '.')
                          , (Wall, '#')
                          ]

