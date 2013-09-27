{-# LANGUAGE OverloadedStrings #-}

module Rogue.Interface (
      module T
    , module Graphics.Vty.Widgets.All
    , showWorld
    , play
    ) where

import Rogue.Types
import qualified Data.Text as T

import Graphics.Vty.Widgets.All
import Text.Printf
import Graphics.Vty.LLInput

-- for now returns a blank screen
showWorld :: Rogue T.Text
showWorld = do
    (maxX, maxY) <- asks screenSize
    return $ T.unlines $ replicate maxY $ T.replicate maxX (T.pack ".")

play :: Rogue ()
play = do
    initialStatus <- getStatusBar
    initialWorld <- showWorld

    r <- liftIO $ createMainInterface initialStatus initialWorld
    let (world, status, fg, c, mainIF) = r

    liftIO $ fg `onKeyPressed` \w k ms ->
        if k == KASCII 'd' && MCtrl `elem` ms then
            shutdownUi >> return True
        else
            return False

    liftIO $ runUi c defaultContext
    
    return ()

getStatusBar :: Rogue T.Text
getStatusBar = do
    Character hp maxHp acc def _ _ <- gets player
    return $ T.pack $ printf "acc:%d | def: %d | hp: %d/%d" acc def hp maxHp

createMainInterface :: T.Text -> T.Text ->
                       IO ( Widget FormattedText
                          , Widget FormattedText
                          , Widget FocusGroup
                          , Collection
                          , IO ()
                          )
createMainInterface initialStatus initialWorld = do
    status     <- plainText initialStatus
    status'    <- hCentered status
    world      <- plainText initialWorld
    basicWorld <- vBox status' world
    fg         <- newFocusGroup
    addToFocusGroup fg basicWorld
    c          <- newCollection
    mainIF     <- addToCollection c basicWorld fg
    return (world, status, fg, c, mainIF)
