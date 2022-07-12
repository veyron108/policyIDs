module Main where

import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import Data.Time.LocalTime
import Data.Time.Format
import System.Console.ANSI
import System.IO
import Data.Fixed
import System.Exit
import System.Directory.Internal.Prelude (exitFailure)


-------------------------------------------------------------------
-- NUMBER STRINGS
-- each bigNum is 10 x 8 characters
-- Notes: manytools.org/hacker-tools/ascii-banner for numbers

bigNum :: Int -> [String]
bigNum 0 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888    888",
     "888    888",
     "888    888",
     "888    888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 1 =
    ["   d888   ",
     "  d8888   ",
     "    888   ",
     "    888   ",
     "    888   ",
     "    888   ",
     "    888   ",
     "  8888888 "]  
bigNum 2 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888    888",
     "    .d88P ",
     ".od888P^  ",
     "d88P^     ",
     "888^      ",
     "8888888888"]
bigNum 3 =
    [" .d8888b. ",
     "d88P  Y88b",
     "     .d88P",
     "    8888^ ",
     "     ^Y8b.",
     "888    888",
     "Y88b  d88P",
     " ^Y888P^  "]
bigNum 4 =
    ["    d8888 ",
     "   d8P888 ",
     "  d8P 888 ",
     " d8P  888 ",
     "d8P   888 ",
     "8888888888",
     "      888 ",
     "      888 "]
bigNum 5 =
    ["888888888 ",
     "888       ",
     "888       ",
     "8888888b. ",
     "     ^Y88b",
     "       888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 6 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888       ",
     "8888888b. ",
     "888P ^Y88b",
     "888    888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 7 =
    ["8888888888",
     "      d88P",
     "      d88P",
     "    d88P  ",
     "   d88P   ",
     "  d88P    ",
     " dBBP     ",
     " dBBP     "]
bigNum 8 =
    [" .d8888b. ",
     "d88P  Y88b",
     "Y88b. d88P",
     " ^Y88888^ ",
     ".d8P^^Y8b.",
     "888    888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 9 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888    888",
     "Y88b. d888",
     " ^Y888P888",
     "       888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum _ = 
    ["          ",
     "          ",
     "          ",
     "          ",
     "          ",
     "          ",
     "          ",
     "          "]

starter :: [String]
starter = ["",
           "",
           "",
           "",
           "",
           "",
           "",
           ""]
          
spacer :: [String]
spacer = [" ",
          " ",
          " ",
          " ",
          " ",
          " ",
          " ",
          " "]

colonOn :: [String]
colonOn  = ["          ",
            "   d88b   ",
            "   Y88P   ",
            "          ",
            "          ",
            "   d8Bb   ",
            "   Y88P   ",
            "          "]

colonOff :: [String]
colonOff = ["          ",
            "          ",
            "          ",
            "          ",
            "          ",
            "          ",
            "          ",
            "          "]

-- must have a \n at the end of each row on the final time string
eol :: [String]
eol = ["\n","\n","\n","\n","\n","\n","\n","\n"]   


-------------------------------------------------------------------
-- DATA TYPES

type Hours       = Int
type Minutes     = Int
type App         = State ClockState

data ClockState = ClockState { 
  asList :: [String], 
  timeOfDay :: TimeOfDay,
  offset :: Minutes 
} deriving Show


-- Get tuple of 10's and 1's and make List
doubleDigits :: Int -> [[String]]
doubleDigits n = [bigNum a] ++ [spacer] ++ [bigNum b]                          -- [Strings] ++ [Strings] ++ [Strings] also works
    where 
      (a, b) = n `divMod` 10                                                      -- divMod :: Integral a => a -> a -> (a, a)


-- Change nested List String to String
convertNestedListToString :: [[String]] -> String
convertNestedListToString n = concat $ concat n


-- Convert from IO ZonedTime to TimeOfDay
convertToTimeOfDay :: ZonedTime -> TimeOfDay                                                     
convertToTimeOfDay obj = localTimeOfDay localtime                                 -- localTimeOfDay :: TimeOfDay
  where 
    localtime = zonedTimeToLocalTime obj                                          -- zonedTimeToLocalTime :: LocalTime

  
-- Need to go from Data.Fixed.Pice E12 to Int
-- Easiest way is to just floor to the nearest Int (downwards)
-- Don't use round as sometimes "60" seconds is shown
convertPicoToInt :: Pico -> Int
convertPicoToInt = floor 


-- Manipulate the List of Strings so they appear horizontally by transposing
-- Notes: Changed from own function to using transpose - forgot that was available earlier - doh!

-- colToRows :: [[a]] -> [[a]]
-- colToRows ([]:_) = []
-- colToRows r = map head r : colToRows (map tail r)

drawClockString :: [[String]] -> String
drawClockString x = convertNestedListToString $ transpose x   -- use transpose x -- same thing!                   


-- i.e. Render Clock String using TimeOfDay as argument 
drawClock :: TimeOfDay -> String
drawClock timeObj = drawClockString output 
  where  
    offset    = getClockStateOffset                                                     -- TimeOfDay has 3 constructors available
    hours     = doubleDigits $ todHour timeObj                                          -- todHour :: Int
    minutes   = doubleDigits $ todMin timeObj                                           -- todHour :: Int
    seconds   = doubleDigits $ convertPicoToInt $ todSec timeObj                        -- todSec :: Pico 
    output    = join [[starter], hours, [colonOn], minutes, [colonOn], seconds, [eol]]  -- "join" the Lists // using Control.Monad                                            


-- getters
getClockStateTime :: ClockState -> TimeOfDay
getClockStateTime ClockState { asList = a, timeOfDay = b, offset = c } = b

getClockStateString :: ClockState -> [String]
getClockStateString ClockState { asList = a, timeOfDay = b, offset = c } = a

getClockStateOffset :: ClockState -> Minutes
getClockStateOffset ClockState { asList = a, timeOfDay = b, offset = c } = c


drawClockState :: State ClockState String 
drawClockState = do
  get >>= \cs ->
    return $ drawClock (getClockStateTime cs)                           -- use getter i.e. TimeOfDay from ClockState


-- Initialize ClockState to 00:00:00, current localtime and no offset
initClock :: IO ClockState
initClock = do
  zonedTime <- getZonedTime                                                     -- getZonedTime :: IO ZonedTime
  let timeOfDay = convertToTimeOfDay zonedTime                                  -- 
  let offsetMinutes = 0
  return $ ClockState {
     asList    = concat $ join [[bigNum 0], [spacer] ,[bigNum 0], [colonOn], [bigNum 0], [spacer], [bigNum 0], [colonOn], [bigNum 0], [spacer], [bigNum 0], [eol]]
    ,timeOfDay = timeOfDay
    ,offset    = offsetMinutes
  }



-- TODO : Change the OFFSET in ClockState - change stream, dont care about previous state
-- updateClockOffsetState :: State ClockState ()
-- updateClockOffsetState = do
--     ...
--     ...
--     return ()


-------------------------------------------------------------------
-- THREADS and EVENTS 
-- See final class notes on 05.07.22 

data Event = TickEvent | KeyEvent Char deriving Show 

ticker :: Chan Event -> IO () 
ticker chan = forever $ do 
  threadDelay (10 ^ 6)                                                          -- every 1 second is 10^6 microseconds
  writeChan chan TickEvent

input :: Chan Event -> IO () 
input chan = forever $ do  
  hSetEcho stdin False
  c <- getChar 
  hSetEcho stdin True
  writeChan chan (KeyEvent c)


-------------------------------------------------------------------
-- i.e. No ENTER required
noBuffering :: IO ()
noBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering


-------------------------------------------------------------------
-- MAIN ROUTINE 
main :: IO ()
main = do 
  noBuffering
  clearScreen
  hideCursor                                        
  hSetEcho stdin False                                                -- ensure the character isn't echoed back to the terminal.

  setCursorPosition 0 0  

  clockState <- initClock                                             -- set the initial state
                                                                      -- putStrLn $ drawClock $ getClockStateTime clockState

  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green] 
  putStrLn $ evalState drawClockState clockState                      -- Note evalState :: State s a -> s -> a


  setCursorPosition 10 0
  putStrLn "===== INSTRUCTIONS ====="
  putStrLn "Press 'x' to quit"
  putStrLn "Press '1' to '3' to change to color"
  putStrLn "Press 'l' for Local time"
  putStrLn "Press 's' for Summer time (+1 hour)"


  chan <- newChan                                                     -- Channels and Events - see last class with fork.hs
  forkIO $ ticker chan 
  forkIO $ input chan
  
  forever $ do 
    c <- readChan chan
    case c of 
      -------- TICKER --------
      TickEvent  -> do

          -- TODO: change this to "put" the timeOfDay into ClockState rather than refresh terminal
          
          setCursorPosition 0 0 

          zonedTime <- getZonedTime  
          let timeOfDay = convertToTimeOfDay zonedTime
          let newState = ClockState { asList = [], timeOfDay = timeOfDay, offset = 30 } 
          --print newState
          putStrLn $ evalState drawClockState newState 


      -------- KEYEVENT --------
      KeyEvent c -> case c of  
        '1' -> do 
                setSGR [SetColor Foreground Vivid Green]
        '2' -> do 
                setSGR [SetColor Foreground Vivid Red]
        '3' -> do 
                setSGR [SetColor Foreground Vivid Yellow]
        'x' -> do                                 -- Exit Program
                putStrLn "Exit Clock"
                showCursor
                exitFailure
        'v' -> do                                 -- Dump the current ClockState to screen
                setCursorPosition 20 0
                print clockState

        -- TODO: Implement offsets
        -- 'l' -> do                              -- Local server time
        --        updateClockOffsetState { offset = 0 }
        -- 's' -> do                              -- SummerTime (add 1 hour) (offset = 60 minutes)

        _ -> putStr "" --putStr("Key : " ++ show [c])
      