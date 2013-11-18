module Rogue.World 
    (
      createWorld
    , positionPlayer
    , showWorld
    , inWorld
    ) where

import Rogue.Types
import Rogue.Util (liftP, subP, addP, randElem, randR, progressBar)

import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Control.Monad (unless, guard, when, forM_, filterM)
import Control.Applicative ((<*>), (<*>), (<$>), pure)
import Data.Array ((!), bounds, array, (//), Array)
import Data.List (delete)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M

import Control.Lens

showWorld :: Rogue String
showWorld = do
    s   <- get
    cfg <- ask
    
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions

    let
        actors     = (s^.player) : (s^.enemies)
        getChar    = getCharAtPos actors (s^.world) (cfg^.worldGlyphs)
        makeLine y = [ getChar (x,y) | x <- [xmin..xmax] ]
        textWorld  = unlines [ makeLine y | y <- [ymin..ymax] ]

    return $ textWorld

        
getCharAtPos :: [Actor] -> World -> WorldGlyphMap -> Position -> Char
getCharAtPos actors w gm pos = 
    if pos `inWorld` w then 
        -- if there is an actor at the position, get its glyph (contained in
        -- the Actor datatype). Otherwise, get whatever is in the world here,
        -- and show it. Otherwise just show a space.
        maybe worldThing (^. glyph) $ actors ^? traversed.filtered (\x-> x^.position == pos)
    else
        ' ' 
  where 
    worldThing = let t = w ! pos in 
            case M.lookup t gm of
                Just (GlyphFunc f) -> f pos w
                Just (Glyph g)     -> g
                _ -> ' '

screenDimensions :: Rogue (Position, Position)
screenDimensions = do
    p <- use (player.position)
    size <- view screenSize
    let dPos = liftP (`div` 2) size
    return (p `subP` dPos, p `addP` dPos)


inWorld :: Position -> Array Position a -> Bool
inWorld (x,y) w = 
    x > 0 && 
    y > 0 && 
    x < maxX && 
    y < maxY
  where
    (_, (maxX, maxY)) = bounds w

inWorldR :: Position -> Rogue Bool
inWorldR p = do
    w <- use world
    return $ p `inWorld` w
    
positionPlayer :: Rogue ()
positionPlayer = do
    possiblePositions <- use floors
    unless (null possiblePositions) $ do
        newPos <- randElem possiblePositions
        player.position .= newPos
        floors %= delete newPos


room :: Size -> [(Position, Thing)]
room s@(maxX, maxY) = 
    [ ((x,y), thing (x,y)) | x <- [0..maxX], y <- [0..maxY] ]
  where
    thing (x,y) | x == 0 || y == 0 || x == maxX || y == maxY = Wall
                 | otherwise = Floor

clearWorld :: Rogue ()
clearWorld = do
    size@(maxX, maxY) <- view worldSize
    let w = array ((0,0), size) $ do
            x <- [0..maxX] 
            y <- [0..maxY]
            return ((x,y),Empty)
    world .= w

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

tunnelDirection :: Position -> Rogue (Maybe Direction)
tunnelDirection pos = do
    w <- use world 
    let ds = do d <- [N,S,E,W] 
                let thing = w ! directionToVector d pos
                guard (thing == Empty) 
                return d
    if not (null ds) then do
        d <- randElem ds
        return (Just d)
    else
        return Nothing

addToWorld :: [(Position, Thing)] -> Rogue ()
addToWorld [] = return ()
addToWorld ((pos, thing):ts) = do
    w <- use world
    when (pos `inWorld` w) $ do
        case w ! pos of
            Empty -> addThing (pos, thing)
            Wall -> when (thing == Floor) $ do
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
        world %= (// [(p,t)])
    removeWall p = do
        walls %= delete p

fitsInWorld :: [(Position, Thing)] -> Rogue Bool
fitsInWorld thgs = do
    w <- use world
    if not $ all (`inWorld` w) (fst <$> thgs) then
        return False
    else do
        overlapAllowed <- view roomOverlapAllowed
        return $ (||) overlapAllowed $ and $ do
            (p,nt) <- thgs
            let wt = w ! p
                b  = wt == Empty || (nt == Wall) && (wt == Wall)
            return b

createRoom :: Direction -> Position -> Rogue Bool
createRoom dir pos@(px,py) = roomLoop 0 
  where 
    roomLoop n = if n >= 3 then return False else do
        min  <- view minRoomSize
        max  <- view maxRoomSize
        size@(rx,ry) <- randR (min, max)
        let r = room size
            
        r' <- case dir of
            N -> do n <- randR (1,rx-1)
                    return $ r & mapped._1 %~ addP (px - n, py - ry)
            S -> do n <- randR (1,rx-1)
                    return $ r & mapped._1 %~ addP (px - n, py)
            E -> do n <- randR (1,ry-1)
                    return $ r & mapped._1 %~ addP (px, py - n)
            W -> do n <- randR (1,ry-1)
                    return $ r & mapped._1 %~ addP (px - rx, py - n)

        fits <- fitsInWorld r'
        if fits then do
            addToWorld r'
            return True
        else
            roomLoop (n+1)        

tunnel :: Position -> Direction -> Rogue Bool
tunnel pos dir = do
    k <- view tunnelThreshold
    n <- randR (0,10)
    if n > k * 10 then do
        ok <- createRoom dir pos
        if ok then do
            addFloor 
            return True
        else
            endTunnel
    else do
        let newPos = directionToVector dir pos
        ok <- inWorldR newPos
        if ok then do
            addFloor
            tunnel newPos dir
        else
            endTunnel
  where 
    otherDirections = if dir `elem` [N,S] then [E,W] else [N,S]
    newDirection = randElem otherDirections
    sideWalls = [(directionToVector d pos, Wall) | d <- otherDirections]
    addFloor = addToWorld $ (pos, Floor) : sideWalls
    endTunnel = do 
        addToWorld $ (pos, Wall) : sideWalls 
        return False

makeInitialRoom :: Rogue ()
makeInitialRoom = do
    dir <- randElem [N,S,E,W]
    maxSize <- view maxRoomSize
    ws <- view worldSize
    pos <- randR ((0,0), ws `subP` maxSize) 
    ok <- createRoom dir pos
    unless ok makeInitialRoom

createWorld :: Rogue ()
createWorld = do
    clearWorld
    makeInitialRoom
    makeTunnels

makeTunnels :: Rogue ()
makeTunnels = do
    max <- view numTunnels
    bar <- progressBar "generating map" max
    forM_ [1..max] $ \n -> when (n < max) $ do
        bar n
        ws <- use walls
        wallsWithAdjacentFloors <- filterM adjacentFloor ws
        theWall <- randElem wallsWithAdjacentFloors
        dir <- tunnelDirection theWall
        when (isJust dir) $ do
            save <- get
            okOnly <- view onlyTerminalTunnels
            ok <- tunnel theWall $ fromJust dir
            when (okOnly && not ok) $ put save
  where 
    adjacentFloor pos = do
        w <- use world
        let things = fmap (w !) (take 4 directionVectors <*> pure pos)
        return $ Floor `elem` things

