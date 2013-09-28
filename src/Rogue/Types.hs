module Rogue.Types where

import Control.Monad.State
import Control.Monad.Reader
import Data.Array
import qualified Data.Map as Map

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    {
      world   :: World
    , enemies :: Map.Map Position Actor
    , player  :: Actor
    }

data RConfig = RConfig 
    {
      worldSize   :: Size
    , screenSize  :: Size
    , worldGlyphs :: WorldGlyphMap
    }

data Actor = Actor 
    {
      hp    :: Int
    , maxHp :: Int
    , acc   :: Int
    , def   :: Int
    , pos   :: Position
    , name  :: String
    , glyph :: Char
    }

type Position      = (Int, Int)
type Size          = (Int, Int)
type World         = Array Position (Maybe Thing)
type WorldGlyphMap = Map.Map Thing Char

data Thing = Floor | Wall
    deriving (Ord, Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Ord, Eq, Show, Enum)

-- Utility functions

liftP :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftP f (ax, ay) (bx, by) = (f ax bx, f ay by)

addP :: Position -> Position -> Position
addP = liftP (+)
