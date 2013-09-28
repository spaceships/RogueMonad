{-# LANGUAGE OverloadedStrings #-}

module Rogue.Interface (
      module T
    , module Graphics.Vty.Widgets.All
    , showWorld
    , play
    ) where

import Rogue.Types
import Rogue.World

import Text.Printf (printf)
import qualified Data.Text as T
import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput

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
    Actor hp maxHp acc def _ _ g <- gets player
    return $ T.pack $ printf "%c | acc:%d | def: %d | hp: %d/%d" g acc def hp maxHp

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
    world'     <- hCentered world
    basicWorld <- vBox status' world'
    fg         <- newFocusGroup
    addToFocusGroup fg basicWorld
    c          <- newCollection
    mainIF     <- addToCollection c basicWorld fg
    return (world, status, fg, c, mainIF)
