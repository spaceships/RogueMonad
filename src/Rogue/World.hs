module Rogue.World where

import Rogue.Types
import Rogue.Util

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Control.Applicative
import Data.Array
import Data.List
import Data.Maybe
import qualified Data.Map as M

showWorld :: Rogue String
showWorld = do
    st  <- get
    cfg <- ask
    
    ((xmin,ymin), (xmax, ymax)) <- screenDimensions

    let
        p                = player st
        actors           = M.insert (position p) p (enemies st)
        getChar          = getCharAtPos actors (world st) (worldGlyphs cfg)
        makeLine y       = [ getChar (x,y) | x <- [xmin..xmax] ]
        textWorld cfg st = unlines [ makeLine y | y <- [ymin..ymax] ]

    return $ textWorld cfg st

        
getCharAtPos :: M.Map Position Actor -> World -> WorldGlyphMap -> Position -> Char
getCharAtPos things w gm pos = 
    if pos `inWorld` w then 
        -- if there is an actor at the position, get its glyph (contained in
        -- the Actor datatype). Otherwise, get whatever is in the world here,
        -- and show it. Otherwise just show a space.
        maybe worldThing glyph $ M.lookup pos things
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
    p <- gets (position . player)
    size <- asks screenSize
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
    w <- gets world
    return $ p `inWorld` w
    
positionPlayer :: Rogue ()
positionPlayer = do
    p <- gets player 
    possiblePositions <- gets floors
    unless (null possiblePositions) $ do
        newPos <- randElem possiblePositions
        modify (\s -> s { player = p { position = newPos } 
                        , floors = delete newPos possiblePositions
                        }) 


room :: Size -> [(Position, Thing)]
room s@(maxX, maxY) = 
    [ ((x,y), thing (x,y)) | x <- [0..maxX], y <- [0..maxY] ]
  where
    thing (x,y) | x == 0 || y == 0 || x == maxX || y == maxY = Wall
                | otherwise = Floor

clearWorld :: Rogue ()
clearWorld = do
    size@(maxX, maxY) <- asks worldSize
    let w = array ((0,0), size) $ do
            x <- [0..maxX] 
            y <- [0..maxY]
            return ((x,y),Empty)
    modify (\s -> s { world = w })

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


directionsAndDirectionVectors :: Position -> [(Direction, Position)]
directionsAndDirectionVectors pos = zip [N,S,E,W,NE,NW,SE,SW] (directionVectors <*> pure pos)

directionToVector :: Direction -> Position -> Position
directionToVector N = directionVectors !! 0
directionToVector S = directionVectors !! 1
directionToVector E = directionVectors !! 2
directionToVector W = directionVectors !! 3

tunnelDirection :: Position -> Rogue (Maybe Direction)
tunnelDirection pos = do
    w <- gets world 
    let ds = do d <- [N,S,E,W] 
                let thing = w ! directionToVector d pos
                guard $ thing == Empty
                return d
    if not (null ds) then do
        d <- randElem ds
        return (Just d)
    else
        return Nothing

addToWorld :: [(Position, Thing)] -> Rogue ()
addToWorld [] = return ()
addToWorld ((pos, thing):ts) = do
    w <- gets world
    when (pos `inWorld` w) $ do
        case w ! pos of
            Empty -> addThing (pos, thing) w
            Wall -> when (thing == Floor) $ do
                removeWall pos
                addThing (pos,thing) w
            _ -> return ()
        addToWorld ts
  where
    addThing (p,t) w = do
        if t == Wall then do
            walls <- gets walls
            unless (p `elem` walls) $ modify (\s -> s { walls = p : walls })
        else do
            floors <- gets floors
            unless (p `elem` floors) $ modify (\s -> s { floors = p : floors })
        modify (\s -> s { world = w // [(p,t)] })
    removeWall p = do
        walls <- gets walls
        modify (\s -> s { walls = delete p walls })
        

fitsInWorld :: [(Position, Thing)] -> Rogue Bool
fitsInWorld thgs = do
    w <- gets world
    if not $ all (`inWorld` w) (fst <$> thgs) then
        return False
    else do
        overlapAllowed <- asks roomOverlapAllowed
        return $ (||) overlapAllowed $ and $ do
            (p,nt) <- thgs
            let wt = w ! p
                b  = wt == Empty || (nt == Wall) && (wt == Wall)
            return b

createRoom :: Direction -> Position -> Rogue Bool
createRoom dir pos@(px,py) = roomLoop 0 
  where 
    roomLoop n = if n >= 3 then return False else do
        min  <- asks minRoomSize
        max  <- asks maxRoomSize
        size@(rx,ry) <- randR (min, max)
        let r = room size
            
        r' <- case dir of
            N -> do n <- randR (1,rx-1)
                    return $ fmap (first (addP (px - n, py - ry))) r
            S -> do n <- randR (1,rx-1)
                    return $ fmap (first (addP (px - n, py))) r
            E -> do n <- randR (1,ry-1)
                    return $ fmap (first (addP (px, py - n))) r
            W -> do n <- randR (1,ry-1)
                    return $ fmap (first (addP (px - rx, py - n))) r

        fits <- fitsInWorld r'
        if fits then do
            addToWorld r'
            return True
        else
            roomLoop (n+1)        

tunnel :: Position -> Direction -> Rogue Bool
tunnel pos dir = do
    k <- asks tunnelThreshold
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
    maxSize <- asks maxRoomSize
    worldSize <- asks worldSize
    pos <- randR ((0,0), worldSize `subP` maxSize) 
    ok <- createRoom dir pos
    unless ok makeInitialRoom

createWorld :: Rogue ()
createWorld = do
    clearWorld
    makeInitialRoom
    makeTunnels

makeTunnels :: Rogue ()
makeTunnels = do
    max <- asks numTunnels
    bar <- progressBar "generating map" max
    forM_ [1..max] $ \n -> when (n < max) $ do
        bar n
        walls <- gets walls
        wallsWithAdjacentFloors <- filterM adjacentFloor walls
        theWall <- randElem wallsWithAdjacentFloors
        dir <- tunnelDirection theWall
        when (isJust dir) $ do
            save <- get
            okOnly <- asks onlyTerminalTunnels
            ok <- tunnel theWall $ fromJust dir
            when (okOnly && not ok) $ put save
  where 
    adjacentFloor pos = do
        w <- gets world
        let things = fmap (w !) (take 4 directionVectors <*> pure pos)
        return $ Floor `elem` things

