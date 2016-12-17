{-# LANGUAGE DeriveDataTypeable, GADTs #-}
module Main where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Operational
import Data.Data
import Graphics.UI.SDL ( Event(..), EventData(..), InitFlag(..), Keysym(keyScancode), KeyMovement(KeyUp, KeyDown), Position(..), Size(..), WindowFlag(WindowShown, WindowOpengl), RenderingDevice(..), RendererFlag(..), addEventWatch, withInit)

import qualified Graphics.UI.SDL as SDL
import System.IO

data Direction
    = Forward
    | Backward
    | TurnLeft
    | TurnRight
    | Stop
      deriving (Eq, Ord, Read, Show, Typeable, Data)

directionChar :: Direction -> Char
directionChar Forward   = 'f'
directionChar Backward  = 'b'
directionChar TurnLeft  = 'l'
directionChar TurnRight = 'r'
directionChar Stop      = 's'

data RobotCommand a where
    WaitEvent     ::                RobotCommand Event
    SendChar      :: Char        -> RobotCommand ()
    GetDirections ::                RobotCommand [Direction]
    SetDirections :: [Direction] -> RobotCommand ()
    LogMsg        :: String      -> RobotCommand ()
    Exit          ::                RobotCommand ()

type Robot = Program RobotCommand

waitEvent :: Robot Event
waitEvent = singleton WaitEvent

sendChar :: Char -> Robot ()
sendChar = singleton . SendChar

getDirections :: Robot [Direction]
getDirections = singleton GetDirections

setDirections :: [Direction] -> Robot ()
setDirections = singleton . SetDirections

logMsg :: String -> Robot ()
logMsg = singleton . LogMsg

sendDirection = sendChar . directionChar

addDirection :: Direction -> Robot ()
addDirection direction =
    do directions <- getDirections
       case directions of
         [] -> do setDirections (direction : directions)
                  sendDirection direction
         (d:ds)
           | direction == d -> return ()
           | otherwise ->
             do setDirections (direction : directions)
                sendDirection direction

removeDirection :: Direction -> Robot ()
removeDirection dir =
    do directions <- getDirections
       dirs <- case directions of
                 [] -> do logMsg "[]1"
                          sendDirection Stop
                          return []
                 dirs@(d:ds)
                     | d == dir ->
                         case ds of
                           [] -> do logMsg "[]2"
                                    sendDirection Stop
                                    return []
                           (d':ds') ->
                               do logMsg "d'"
                                  sendDirection d'
                                  return ds
                     | otherwise ->
                         do logMsg $ "o(" ++ show dirs ++")"
                            return $ filter (/= dir) dirs
       setDirections $ dirs
       return ()
{-
sdlDummy :: Bool -> Robot a -> IO a
sdlDummy enableLog robot' =
    withInit [InitVideo] $
        do surface <- setVideoMode 640 480 24 []
           go [] robot'
    where
      go :: [Direction] -> Robot a -> IO a
      go directions robot =
          case view robot of
            Return a -> return a
            (WaitEvent :>>= k) ->
                do event <- SDL.waitEvent
                   go directions (k event)
            (SendChar c :>>= k) ->
                do putChar c
                   go directions (k ())
            (GetDirections :>>= k) ->
                do go directions (k directions)
            (SetDirections newDirections :>>= k) ->
                do go newDirections (k ())
            (LogMsg msg :>>= k) ->
                do when enableLog $ putStr msg
                   go directions (k ())
-}

waitEventChan :: TChan Event -> IO Event
waitEventChan evChan = atomically $ readTChan evChan

createEventChan :: IO (TChan Event)
createEventChan =
  do c <- newTChanIO
--     addEventWatch (\ev -> do print ev
--                              atomically $ writeTChan c ev)
     return c

pollEvent' :: IO Event
pollEvent' =
  do mEvent <- SDL.pollEvent
     case mEvent of
       Nothing -> pollEvent'
       (Just ev) -> return ev

sdlRun :: Handle
       -> Bool
       -> Robot ()
       -> IO ()
sdlRun handle enableLog robot' =
    withInit [InitEvents, InitVideo] $ do
      SDL.withWindow "robot command center" (Position 0 0) (Size 640 480) [WindowShown, WindowOpengl] $ \window ->
        SDL.withOpenGL window $ do
          SDL.withRenderer window FirstSupported [PresentVSync] $ \render -> do
            evChan <- createEventChan
            go evChan [] robot'
    where
      go :: TChan Event -> [Direction] -> Robot () -> IO ()
      go evChan directions robot =
          case view robot of
            Return a -> return a
            (WaitEvent :>>= k) ->
                do -- event <- waitEventChan evChan
                   event <- pollEvent'
                   print event
                   go evChan directions (k event)
            (SendChar c :>>= k) ->
                do hPutChar handle c
                   go evChan directions (k ())
            (GetDirections :>>= k) ->
                do go evChan directions (k directions)
            (SetDirections newDirections :>>= k) ->
                do go evChan newDirections (k ())
            (LogMsg msg :>>= k) ->
                do when enableLog $ putStr msg
                   go evChan directions (k ())
            (Exit :>>= _) ->
              do hPutChar handle 's'
                 return ()

eventLoop :: Robot ()
eventLoop =
    do (Event _ eventData) <- waitEvent
       case eventData of
         (Keyboard movement _ _ keySym) ->
           let f = case movement of
                 KeyDown -> addDirection
                 KeyUp   -> removeDirection
           in case keyScancode keySym of
               SDL.Up    -> f Forward
               SDL.Down  -> f Backward
               SDL.Left  -> f TurnLeft
               SDL.Right -> f TurnRight
               SDL.Q     -> singleton Exit
               _          -> return ()
         _ ->
             return ()
       eventLoop

main =
    do putStrLn "Attempting to open /dev/rfcomm0."
       withFile "/dev/rfcomm0" ReadWriteMode $ \handle ->
         do putStrLn "Connection to rfcomm0 created."
            hSetBuffering handle NoBuffering
            putStrLn "Disabled Buffering."
            sdlRun handle True eventLoop

{-
main = sdlDummy False eventLoop



{-
addDirection, removeDirection
    :: (Functor m, MonadIO m) =>
       Direction
    -> StateT RoverState m ()
addDirection dir =
    do modify $ \rs -> rs { direction = dir : (direction rs) }
       sendDirection dir
       return ()
-}

{-
main :: IO ()
main =
    withRoverState $ \roverState ->
        withInit [InitEverything] $
             do e <- getError
                print e
                surface <- setVideoMode 640 480 24 []
                evalStateT eventLoop roverState


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
-}
-}
