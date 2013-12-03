{-# LANGUAGE TemplateHaskell #-}

module Rogue.World 
    (
      randomWorld
    , inWorld
    ) where

import Rogue.Types
import Rogue.Util

import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Applicative
import Data.Array
import Data.List (delete)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M

import Control.Lens

type WorldGen = StateT WorldGenSt (Reader WorldGenCfg)
runWorldGen :: WorldGen a -> WorldGenSt -> WorldGenCfg -> a
runWorldGen m st cfg = runReader (evalStateT m st) cfg

data WorldGenSt = WorldGenSt
    { _partialWorld :: World
    , _stdGenW :: StdGen
    , _floors :: [Position]
    , _walls :: [Position]
    }

data WorldGenCfg = WorldGenCfg
    { _worldSize :: Size
    , _minRoomSize :: Size
    , _maxRoomSize :: Size
    , _tunnelThreshold :: Float
    , _roomOverlapAllowed :: Bool
    , _numTunnels :: Int
    , _onlyTerminalTunnels :: Bool
    }

makeLenses ''WorldGenSt
makeLenses ''WorldGenCfg

randW :: Random a => (a,a) -> WorldGen a
randW range = do
    g <- use stdGenW
    let (a, g') = randomR range g
    stdGenW .= g'
    return a

randElemW :: [a] -> WorldGen a
randElemW xs = do
    n <- randW (0, length xs - 1)
    return (xs !! n)

randomWorld :: StdGen -> World
randomWorld g = runWorldGen createWorld st cfg
  where
    (cfg, g') = runState randWorldGenCfg g
    st = WorldGenSt 
            { _partialWorld = array ((0,0),(0,0)) [((0,0),EmptySpace)]
            , _stdGenW = g'
            , _floors = []
            , _walls = []
            }

randWorldGenCfg :: State StdGen WorldGenCfg
randWorldGenCfg = do
    ws <- randSizeSt ((50,25),(200,100)) 
    minRs <- randSizeSt ((3,3),(20,10))
    maxRs <- randSizeSt (minRs,(30,20))
    tt <- randRSt (0.10,0.70)
    roa <- randSt
    num <- randRSt (20,70)
    otts <- randSt
    return WorldGenCfg { _worldSize = ws
                       , _minRoomSize = minRs
                       , _maxRoomSize = maxRs
                       , _tunnelThreshold = tt
                       , _roomOverlapAllowed = roa
                       , _numTunnels = num
                       , _onlyTerminalTunnels = otts
                       }
     
randSt :: Random a => State StdGen a
randSt = do
    g <- get
    let (x, g') = random g
    put g'
    return x

randRSt :: Random a => (a,a) -> State StdGen a
randRSt range = do
    g <- get
    let (x, g') = randomR range g
    put g'
    return x

randSizeSt :: Random a => ((a,a),(a,a)) -> State StdGen (a,a)
randSizeSt ((x1,y1),(x2,y2)) = do
    x <- randRSt (x1,x2)
    y <- randRSt (y1,y2)
    return (x,y)

inWorld :: Position -> World -> Bool
inWorld (x,y) w = 
    x > 0 && 
    y > 0 && 
    x < maxX && 
    y < maxY
  where
    (_, (maxX, maxY)) = bounds w

inWorldW :: Position -> WorldGen Bool
inWorldW p = do
    w <- use partialWorld
    return $ p `inWorld` w
    
room :: Size -> [(Position, Thing)]
room s@(maxX, maxY) = 
    [ ((x,y), thing (x,y)) | x <- [0..maxX], y <- [0..maxY] ]
  where
    thing (x,y) | x == 0 || y == 0 || x == maxX || y == maxY = Wall
                | otherwise = emptyFloor

clearWorld :: WorldGen ()
clearWorld = do
    size@(maxX, maxY) <- view worldSize
    let w = array ((0,0), size) $ do
            x <- [0..maxX] 
            y <- [0..maxY]
            return ((x,y), EmptySpace)
    partialWorld .= w

directionVectors :: [Position -> Position]
directionVectors = [north,south,east,west,northeast,northwest,southeast,southwest]
  where 
    north = addP (0,-1)
    south = addP (0,1)
    east  = addP (1,0)
    west  = addP (-1,0)
    northeast = addP (1,-1)
    northwest = addP (-1,-1)
    southeast = addP (1,1)
    southwest = addP (-1,1)


--directionsAndDirectionVectors :: Position -> [(Direction, Position)]
--directionsAndDirectionVectors pos = zip [N,S,E,W,NE,NW,SE,SW] (directionVectors <*> pure pos)

directionToVector :: Direction -> Position -> Position
directionToVector N = directionVectors !! 0
directionToVector S = directionVectors !! 1
directionToVector E = directionVectors !! 2
directionToVector W = directionVectors !! 3

tunnelDirection :: Position -> WorldGen (Maybe Direction)
tunnelDirection pos = do
    w <- use partialWorld 
    let ds = do d <- [N,S,E,W] 
                let thing = w ! directionToVector d pos
                guard (thing == EmptySpace) 
                return d
    if not (null ds) then do
        d <- randElemW ds
        return (Just d)
    else
        return Nothing

addToWorld :: [(Position, Thing)] -> WorldGen ()
addToWorld [] = return ()
addToWorld ((pos, thing):ts) = do
    w <- use partialWorld
    when (pos `inWorld` w) $ do
        case w ! pos of
            EmptySpace -> addThing (pos, thing)
            Wall -> when (isFloor thing) $ do
                removeWall pos
                addThing (pos,thing)
            _ -> return ()
        addToWorld ts
  where
    addThing (p,t) = do
        if t == Wall then do
            ws <- use walls
            unless (p `elem` ws) $ walls %= (p:)
        else do
            fs <- use floors
            unless (p `elem` fs) $ floors %= (p:)
        partialWorld %= (// [(p,t)])
    removeWall p = do
        walls %= delete p

fitsInWorld :: [(Position, Thing)] -> WorldGen Bool
fitsInWorld thgs = do
    w <- use partialWorld
    if not $ all (`inWorld` w) (fst <$> thgs) then
        return False
    else do
        overlapAllowed <- view roomOverlapAllowed
        return $ (||) overlapAllowed $ and $ do
            (p,nt) <- thgs
            let wt = w ! p
                b  = wt == EmptySpace || (nt == Wall) && (wt == Wall)
            return b

createRoom :: Direction -> Position -> WorldGen Bool
createRoom dir pos@(px,py) = roomLoop 0 
  where 
    roomLoop n = if n >= 3 then return False else do
        min  <- view minRoomSize
        max  <- view maxRoomSize
        size@(rx,ry) <- randW (min, max)
        let r = room size
            
        r' <- case dir of
            N -> do n <- randW (1,rx-1)
                    return $ r & mapped._1 %~ addP (px - n, py - ry)
            S -> do n <- randW (1,rx-1)
                    return $ r & mapped._1 %~ addP (px - n, py)
            E -> do n <- randW (1,ry-1)
                    return $ r & mapped._1 %~ addP (px, py - n)
            W -> do n <- randW (1,ry-1)
                    return $ r & mapped._1 %~ addP (px - rx, py - n)

        fits <- fitsInWorld r'
        if fits then do
            addToWorld r'
            return True
        else
            roomLoop (n+1)        

tunnel :: Position -> Direction -> WorldGen Bool
tunnel pos dir = do
    k <- view tunnelThreshold
    n <- randW (0,10)
    if n > k * 10 then do
        ok <- createRoom dir pos
        if ok then do
            addFloor 
            return True
        else
            endTunnel
    else do
        let newPos = directionToVector dir pos
        ok <- inWorldW newPos
        if ok then do
            addFloor
            tunnel newPos dir
        else
            endTunnel
  where 
    otherDirections = if dir `elem` [N,S] then [E,W] else [N,S]
    newDirection = randElemW otherDirections
    sideWalls = [(directionToVector d pos, Wall) | d <- otherDirections]
    addFloor = addToWorld $ (pos, emptyFloor) : sideWalls
    endTunnel = do 
        addToWorld $ (pos, Wall) : sideWalls 
        return False

makeInitialRoom :: WorldGen ()
makeInitialRoom = do
    dir <- randElemW [N,S,E,W]
    maxSize <- view maxRoomSize
    ws <- view worldSize
    pos <- randW ((0,0), ws `subP` maxSize) 
    ok <- createRoom dir pos
    unless ok makeInitialRoom

createWorld :: WorldGen World
createWorld = do
    clearWorld
    makeInitialRoom
    makeTunnels
    w <- use partialWorld
    return w

makeTunnels :: WorldGen ()
makeTunnels = do
    max <- view numTunnels
    forM_ [1..max] $ \n -> when (n < max) $ do
        ws <- use walls
        wallsWithAdjacentFloors <- filterM adjacentFloor ws
        theWall <- randElemW wallsWithAdjacentFloors
        dir <- tunnelDirection theWall
        when (isJust dir) $ do
            save <- get
            okOnly <- view onlyTerminalTunnels
            ok <- tunnel theWall $ fromJust dir
            when (okOnly && not ok) $ put save
  where 
    adjacentFloor pos = do
        w <- use partialWorld
        let things = fmap (w !) (take 4 directionVectors <*> pure pos)
        return $ any isFloor things

