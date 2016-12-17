{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Data
import Graphics.UI.SDL
import System.IO

main :: IO ()
main =
    withRoverState $ \roverState ->
        withInit [InitEverything] $
             do e <- getError
                print e
                surface <- setVideoMode 640 480 24 []
                evalStateT eventLoop roverState

data Direction
    = Forward
    | Backward
    | TurnLeft
    | TurnRight
    | Stop
      deriving (Eq, Ord, Read, Show, Typeable, Data)

data RoverState = RoverState
    { communication :: Handle
    , direction     :: [Direction]
    }

withRoverState :: (RoverState -> IO a)
               -> IO a
withRoverState f =
    do putStrLn "Attempting to open /dev/rfcomm0."
       withFile "/dev/rfcomm0" ReadWriteMode $ \handle ->
--       let handle = stdout in
         do putStrLn "Connection to rfcomm0 created."
            hSetBuffering handle NoBuffering
            putStrLn "Disabled Buffering."
            f (RoverState { communication = handle
                          , direction     = []
                          })

eventLoop :: StateT RoverState IO ()
eventLoop =
    do event <- liftIO waitEvent
       -- liftIO $ print event
       case event of
         (KeyDown keySym) ->
             case symKey keySym of
               SDLK_UP    -> addDirection Forward
               SDLK_DOWN  -> addDirection Backward
               SDLK_LEFT  -> addDirection TurnLeft
               SDLK_RIGHT -> addDirection TurnRight
               _          -> return ()
         (KeyUp keySym) ->
             case symKey keySym of
               SDLK_UP    -> removeDirection Forward
               SDLK_DOWN  -> removeDirection Backward
               SDLK_LEFT  -> removeDirection TurnLeft
               SDLK_RIGHT -> removeDirection TurnRight
               _          -> return ()
         _ ->
             return ()
       eventLoop

addDirection, removeDirection
    :: (Functor m, MonadIO m) =>
       Direction
    -> StateT RoverState m ()
addDirection dir =
    do modify $ \rs -> rs { direction = dir : (direction rs) }
       sendDirection dir
       return ()

removeDirection dir =
    do rs <- get
       dirs <- case direction rs of
                 [] -> do -- liftIO $ putStr "[]1"
                          sendDirection Stop
                          return []
                 dirs@(d:ds)
                     | d == dir ->
                         case ds of
                           [] -> do -- liftIO $ putStr "[]2"
                                    sendDirection Stop
                                    return []
                           (d':ds') ->
                               do -- liftIO $ putStr "d'"
                                  sendDirection d'
                                  return ds
                     | otherwise ->
                         do -- liftIO $ putStr $ "o(" ++ show dirs ++")"
                            return $ filter (/= dir) dirs
       put $ rs { direction = dirs }
       return ()

sendDirection :: (Functor m, MonadIO m) =>
                 Direction
              -> StateT RoverState m ()
sendDirection Forward   = sendChar 'f'
sendDirection Backward  = sendChar 'b'
sendDirection TurnLeft  = sendChar 'l'
sendDirection TurnRight = sendChar 'r'
sendDirection Stop      = sendChar 's'

sendChar :: (Functor m, MonadIO m) =>
            Char
         -> StateT RoverState m ()
sendChar c =
    do h <- communication <$> get
       liftIO $ hPutChar h c

forward, backward, left, right, stop :: (Functor m, MonadIO m) => StateT RoverState m ()
forward  = sendChar 'f'
backward = sendChar 'b'
left     = sendChar 'l'
right    = sendChar 'r'
stop     = sendChar 's'