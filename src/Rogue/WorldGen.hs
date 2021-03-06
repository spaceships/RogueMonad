{-# LANGUAGE TemplateHaskell #-}

module Rogue.WorldGen
    (
      randomWorld
    , randomWorldR
    ) where

import Rogue.Types
import Rogue.Util

import Data.List (delete)
import Data.Maybe (mapMaybe, fromJust, isJust, isNothing)
import qualified Data.Map as M
import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad
import Control.Applicative
import Control.Lens

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
    , _depthW :: Int
    , _maxDepthW :: Int
    , _numStairsW :: Int
    }

type WorldGen = StateT WorldGenSt (Reader WorldGenCfg)
runWorldGen :: WorldGen a -> WorldGenSt -> WorldGenCfg -> (a, StdGen)
runWorldGen m st cfg = (a, _stdGenW s)
  where
    (a, s) = runReader (runStateT m st) cfg

$( makeLenses ''WorldGenSt )
$( makeLenses ''WorldGenCfg )

randomWorldR :: Rogue World
randomWorldR = do
    g <- use stdGenR
    d <- use depth
    maxD <- view numLevels
    nstairs <- view numStairs
    let (w, g') = randomWorld (d+1) maxD nstairs g
    stdGenR .= g'
    return w

randomWorld :: Int -> Int -> Int -> StdGen -> (World, StdGen)
randomWorld depth maxDepth numStairs g = runWorldGen createWorld st cfg'
  where
    (cfg, g') = runState randWorldGenCfg g
    cfg' = cfg { _depthW = depth 
               , _maxDepthW = maxDepth 
               , _numStairsW = numStairs
               }
    st = WorldGenSt
            { _partialWorld = emptyWorld
            , _stdGenW = g'
            , _floors = []
            , _walls = []
            }

inWorldW :: Position -> WorldGen Bool
inWorldW (x,y) = do
    (maxX,maxY) <- view worldSize
    return (x <= maxX && y <= maxY) 

createWorld :: WorldGen World
createWorld = do
    clearWorld
    makeInitialRoom
    makeTunnels

    n <- view numStairsW
    d <- view depthW
    replicateM_ n $ positionStructure StairsUp
    maxD <- view maxDepthW 
    unless (d >= maxD) $ replicateM_ n (positionStructure StairsDown)

    w <- use partialWorld
    return w

randWorldGenCfg :: State StdGen WorldGenCfg
randWorldGenCfg = do
    ws <- randSizeSt ((50,25),(200,100))
    minRs <- randSizeSt ((3,3),(10,7))
    maxRs <- randSizeSt (minRs,(20,15))
    tt <- randRSt (0.40,0.90)
    roa <- randSt
    num <- randRSt (20,70)
    otts <- randSt
    return WorldGenCfg 
        { _worldSize = ws
        , _depthW = 0
        , _maxDepthW = 0
        , _numStairsW = 0
        , _minRoomSize = minRs
        , _maxRoomSize = maxRs
        , _tunnelThreshold = tt
        , _roomOverlapAllowed = roa
        , _numTunnels = num
        , _onlyTerminalTunnels = otts
        }

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

clearWorld :: WorldGen ()
clearWorld = partialWorld .= M.empty

addToWorld :: [(Position, Thing)] -> WorldGen ()
addToWorld = (^! folded.act addThing)
  where
    addThing (p,t) = do
        w  <- use partialWorld
        let wt = w^.at p
        -- Floor > Wall > Empty
        if isFloor t then do
            unless (isJustFloor wt) $ do
                floors %= (p:)
                partialWorld.at p ?= t
            when (isJustWall wt) $ walls %= delete p
        else when (isWall t) $ do
            unless (isJustFloor wt || isJustWall wt) $ do
                walls %= (p:)
                partialWorld.at p ?= t

fitsInWorld :: [(Position, Thing)] -> WorldGen Bool
fitsInWorld things = do
    w <- use partialWorld
    -- Is every tile of tngs contained within worldSize?
    inBounds <- and <$> mapM inWorldW (fst <$> things)
    -- Can rooms overlap?
    overlapAllowed <- view roomOverlapAllowed
    -- It's okay for walls to overlap when overlap is not allowed
    let walls     = things ^.. traversed.filtered (isWall.snd)
        overlaps  = mapMaybe (\(i,_)-> w^.at i) walls
        onlyWalls = all isWall overlaps
    return $ inBounds && (overlapAllowed || onlyWalls)

room :: Size -> [(Position, Thing)]
room s@(maxX, maxY) =
    [ ((x,y), thing (x,y)) | x <- [0..maxX], y <- [0..maxY] ]
  where
    thing (x,y) | x == 0 || y == 0 || x == maxX || y == maxY = Wall
                | otherwise = emptyFloor

createRoom :: Direction -> Position -> WorldGen Bool
createRoom dir (px,py) = roomLoop 0
  where
    roomLoop n = if n >= 3 then return False else do
        min  <- view minRoomSize
        max  <- view maxRoomSize
        (rx,ry) <- randW (min, max)
        let r = room (rx,ry)

        r' <- case dir of
            N -> do n <- randW (1,rx-1)
                    return $ r & mapped._1 %~ addP (px-n,py-ry)
            S -> do n <- randW (1,rx-1)
                    return $ r & mapped._1 %~ addP (px-n,py)
            E -> do n <- randW (1,ry-1)
                    return $ r & mapped._1 %~ addP (px,py-n)
            W -> do n <- randW (1,ry-1)
                    return $ r & mapped._1 %~ addP (px-rx,py-n)

        fits <- fitsInWorld r'
        if fits then do
            addToWorld r'
            return True
        else
            roomLoop (n+1)

makeInitialRoom :: WorldGen ()
makeInitialRoom = do
    dir <- randElemW [N,S,E,W]
    maxSize <- view maxRoomSize
    ws <- view worldSize
    pos <- randW ((0,0), ws `subP` maxSize)
    ok <- createRoom dir pos
    unless ok makeInitialRoom

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

tunnelDirection :: Position -> WorldGen (Maybe Direction)
tunnelDirection pos = do
    w <- use partialWorld
    let ds = do d <- [N,S,E,W]
                let thing = w ^. at (directionToVector d pos)
                guard (isNothing thing)
                return d
    if not (null ds) then do
        d <- randElemW ds
        return (Just d)
    else
        return Nothing

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
        let things = mapMaybe (\i -> w ^. at i) (take 4 directionVectors <*> pure pos)
        return $ any isFloor things

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

directionToVector :: Direction -> Position -> Position
directionToVector N = directionVectors !! 0
directionToVector S = directionVectors !! 1
directionToVector E = directionVectors !! 2
directionToVector W = directionVectors !! 3

positionStructure :: Structure -> WorldGen ()
positionStructure s = do
    ps <- gets (^@.. partialWorld.itraversed
                   . filtered isFloor
                   . filtered hasNoStructure)
    unless (null ps) $ do
        p <- fst <$> randElemW ps
        partialWorld.ix p.structure ?= s
