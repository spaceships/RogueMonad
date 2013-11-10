module Rogue.Util where

import Rogue.Types

import System.Random (Random, random, randomR)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Control.Monad.State (gets, modify)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)

liftP :: (a -> b) -> (a, a) -> (b, b)
liftP f (a, b) = (f a, f b)

liftP2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
liftP2 f (ax, ay) (bx, by) = (f ax bx, f ay by)

addP :: Position -> Position -> Position
addP = liftP2 (+)

subP :: Position -> Position -> Position
subP = liftP2 (-)

rand :: Random a => Rogue a
rand = do
    g <- gets stdGen 
    let (a, g') = random g
    modify $ \s -> s { stdGen = g' }
    return a

randR :: Random a => (a,a) -> Rogue a
randR range = do
    g <- gets stdGen 
    let (a, g') = randomR range g
    modify $ \s -> s { stdGen = g' }
    return a

randElem :: [a] -> Rogue a
randElem xs = do
    n <- randR (0, length xs - 1)
    return (xs !! n)

printR :: Show a => a -> Rogue ()
printR = liftIO . print

center :: String -> Int -> String
center s w = replicate left ' ' ++ s ++ replicate right ' '
  where
    l = length s
    toBeFilled = w - l
    (both, leftAdd) = toBeFilled `divMod` 2
    (left, right) = (both + leftAdd, both)

progressBar :: String -> Int -> Rogue (Int -> Rogue ())
progressBar label total = do
    liftIO clearScreen
    (maxX,maxY) <- asks screenSize
    let length = 20
        y = maxY `div` 2
    liftIO $ setCursorPosition (y-1) 0
    liftIO $ putStrLn (center label maxX)
    return $ \n -> do
        liftIO $ setCursorPosition y 0
        let p = floor ((fromIntegral n / fromIntegral total) 
                        * fromIntegral length)
            s = "|" ++ replicate p '=' ++ replicate (length - p) ' ' ++ "|"
        liftIO $ putStr $ center s maxX
        
