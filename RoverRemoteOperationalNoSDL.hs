{-# LANGUAGE DeriveDataTypeable, GADTs #-}
module Main where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Operational
import Data.Data
-- import Graphics.UI.SDL ( InitFlag(InitVideo), SDLKey(..), Event(..), withInit
--                       , setVideoMode, symKey)
-- import qualified Graphics.UI.SDL as SDL
import System.IO
import qualified UI.NCurses as N

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
    WaitEvent     ::                RobotCommand  Event
    SendChar      :: Char        -> RobotCommand ()
    GetDirections ::                RobotCommand [Direction]
    SetDirections :: [Direction] -> RobotCommand ()
    LogMsg        :: String      -> RobotCommand ()
    Quit          ::             -> RobotCommand ()

type Robot = Program RobotCommand

data Key = UP | DOWN | LEFT | RIGHT
data Event = KeyDown Key | KeyUp Key

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
       setDirections (direction : directions)
       sendDirection direction
       return ()

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
eventLoop :: Robot ()
eventLoop =
    do event <- waitEvent
       case event of
         (KeyDown keySym) ->
             case keySym of
               UP    -> addDirection Forward
               DOWN  -> addDirection Backward
               LEFT  -> addDirection TurnLeft
               RIGHT -> addDirection TurnRight
--               _          -> return ()
         (KeyUp keySym) ->
             case keySym of
               UP    -> removeDirection Forward
               DOWN  -> removeDirection Backward
               LEFT  -> removeDirection TurnLeft
               RIGHT -> removeDirection TurnRight
--               _          -> return ()
--         _ ->
--             return ()
       eventLoop


{-
cliEvent :: IO Event
cliEvent =
  do c <- getChar
     case c of
       'f' -> return (KeyDown UP)
       ' ' -> return (KeyUp UP)
       _   -> cliEvent

cli :: Bool -> Robot a -> IO a
cli enableLog robot =
  do hSetBuffering  stdin NoBuffering
     putStrLn "Attempting to open /dev/rfcomm0."
     withFile "/dev/rfcomm0" ReadWriteMode $ \handle ->
         do putStrLn "Connection to rfcomm0 created."
            hSetBuffering handle NoBuffering
            putStrLn "Disabled Buffering."
            go handle [] robot
  where
    go :: Handle -> [Direction] -> Robot a -> IO a
    go handle directions robot =
      case view robot of
        Return a -> return a
        (WaitEvent :>>= k) ->
          do event <- cliEvent
             go handle directions (k event)
        (SendChar c :>>= k) ->
          do hPutChar handle c
             go handle directions (k ())
        (GetDirections :>>= k) ->
          do go handle directions (k directions)
        (SetDirections newDirections :>>= k) ->
          do go handle newDirections (k ())
        (LogMsg msg :>>= k) ->
          do when enableLog $ putStr msg
             go handle directions (k ())
-}

ncursesEvent :: N.Window -> N.NCurses Event
ncursesEvent window =
  do e <- N.getEvent window
     case c of
       'f' -> return (KeyDown UP)
       ' ' -> return (KeyUp UP)
       _   -> cliEvent

ncurses :: Bool -> Robot a -> IO a
ncurses enableLog robot =
  do putStrLn "Attempting to open /dev/rfcomm0."
     withFile "/dev/rfcomm0" ReadWriteMode $ \handle ->
         do putStrLn "Connection to rfcomm0 created."
            hSetBuffering handle NoBuffering
            putStrLn "Disabled Buffering."
            N.runCurses $ do
              w <- N.newWindow 0 0 0 0
              go handle [] robot
              N.closeWindow w

  where
    go :: Handle -> [Direction] -> Robot a -> N.Curses a
    go handle directions robot =
      case view robot of
        Return a -> return a
        (WaitEvent :>>= k) ->
          do event <- cliEvent
             go handle directions (k event)
        (SendChar c :>>= k) ->
          do hPutChar handle c
             go handle directions (k ())
        (GetDirections :>>= k) ->
          do go handle directions (k directions)
        (SetDirections newDirections :>>= k) ->
          do go handle newDirections (k ())
        (LogMsg msg :>>= k) ->
          do when enableLog $ putStr msg
             go handle directions (k ())

main :: IO ()
main = cli False eventLoop



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
               UP    -> addDirection Forward
               DOWN  -> addDirection Backward
               LEFT  -> addDirection TurnLeft
               RIGHT -> addDirection TurnRight
               _          -> return ()
         (KeyUp keySym) ->
             case symKey keySym of
               UP    -> removeDirection Forward
               DOWN  -> removeDirection Backward
               LEFT  -> removeDirection TurnLeft
               RIGHT -> removeDirection TurnRight
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
