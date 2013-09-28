module Rogue.Types where

import Data.Array
import Control.Monad.State
import Control.Monad.Reader
import Graphics.Vty.LLInput

import qualified Data.Map as M

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    {
      world   :: World
    , enemies :: M.Map Position Actor
    , player  :: Actor
    }

data RConfig = RConfig 
    {
      worldSize   :: Size
    , screenSize  :: Size
    , worldGlyphs :: WorldGlyphMap
    , bindings    :: Bindings
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

type Bindings      = [((Key, [Modifier]), Rogue ())]
type Position      = (Int, Int)
type Size          = (Int, Int)
type World         = Array Position (Maybe Thing)
type WorldGlyphMap = M.Map Thing Char

data Thing = Floor | Wall
    deriving (Ord, Show, Eq)

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Ord, Eq, Show, Enum)

-- Utility functions -- should probably go somewhere else

liftP :: (a -> b) -> (a, a) -> (b, b)
liftP f (a, b) = (f a, f b)

liftP2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftP2 f (ax, ay) (bx, by) = (f ax bx, f ay by)

addP :: Position -> Position -> Position
addP = liftP2 (+)

subP :: Position -> Position -> Position
subP = liftP2 (-)
