module Rogue.Types where

import Data.Array (Array)
import System.Random (Random, StdGen, Random, random, randomR)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Reader (ReaderT, runReaderT)

import qualified Data.Map as M

type Rogue = StateT RState (ReaderT RConfig IO)
runRogue :: Rogue () -> RConfig -> RState -> IO ()
runRogue m c s = runReaderT (evalStateT m s) c

data RState = RState 
    {
      world   :: World
    , enemies :: M.Map Position Actor
    , player  :: Actor
    , done    :: Bool
    , stdGen  :: StdGen
    , floors  :: [Position]
    , walls   :: [Position]
    }

data RConfig = RConfig 
    {
      worldSize   :: Size
    , minRoomSize :: Size
    , maxRoomSize :: Size
    , screenSize  :: Size
    , worldGlyphs :: WorldGlyphMap
    , bindings    :: Bindings
    , tunnelThreshold :: Float
    , roomOverlapAllowed :: Bool
    , numTunnels :: Int
    , onlyTerminalTunnels :: Bool
    }

data Actor = Actor 
    {
      hp       :: Int
    , maxHp    :: Int
    , acc      :: Int
    , def      :: Int
    , position :: Position
    , name     :: String
    , glyph    :: Char
    }

type Bindings      = [(Char, Rogue ())]
type Position      = (Int, Int)
type Size          = (Int, Int)
type World         = Array Position Thing
type WorldGlyphMap = M.Map Thing WorldGlyph

data Thing = Floor 
           | Wall 
           | Empty
    deriving (Ord, Show, Eq)

data WorldGlyph = Glyph Char                            -- Static character
                | GlyphFunc (Position -> World -> Char) -- Dynamic character

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Ord, Eq, Show, Enum)

instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)
    random gen1 = let (x, gen2) = random gen1
                      (y, gen3) = random gen2
                  in ((x,y), gen3)
