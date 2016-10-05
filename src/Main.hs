-- This is the main file which drives the program
-- The is nothing in here you need to worry about

import Control.Monad (when)
import Data.Maybe
import Data.Time.Clock
import System.Console.ParseArgs
import System.Exit

import Datastructures.Ant
import Datastructures.Cells
import Datastructures.SquareWorld
import Datastructures.HexWorld
import Datastructures.Transitions

import Drawing.World
import Graphics.Gloss.Interface.Pure.Simulate

import StudentSources.LangtonsHex as Hex
import StudentSources.LangtonsAnt as Ant

-- Parsing command line arguments. If you're doing some adventurous
-- extension you might need to look at this.
data ArgFlag = Mode | Trans | FPS | Skip | Start | Simulations | Help
             | InitialCol | ColShift
  deriving (Show, Eq, Ord)

langtonArguments :: [Arg ArgFlag]
langtonArguments =
  [ Arg Mode (Just 'm') (Just "mode") (argDataDefaulted "mode" ArgtypeString "part0")
      "The grid mode. Either \"square\", \"hex\" or \"part0\" "
  , Arg Trans (Just 't') (Just "transition") (argDataOptional "transition" ArgtypeString)
      "The transition system to use. i.e \"LRRL\""
  , Arg FPS (Just 'f') (Just "fps") (argDataDefaulted "fps" ArgtypeInt 10)
      "How many frames to draw a second"
  , Arg Skip (Just 's') (Just "skip") (argDataDefaulted "skip" ArgtypeInt 1)
      "How many transition to do per frame"
  , Arg Start Nothing (Just "start") (argDataDefaulted "start" ArgtypeInt 0)
      "How many generations to run before drawing"
  , Arg Simulations Nothing (Just "time") (argDataDefaulted "time" ArgtypeInt 5000)
      "How many transitions to time"
  , Arg InitialCol (Just 'c') (Just "colour") (argDataDefaulted "colour" ArgtypeInt 240)
      "What colour is colour 1 (expressed as an angle in degrees in HSV space)"
  , Arg ColShift Nothing (Just "shift") (argDataDefaulted "shift" ArgtypeInt 31)
      "What angle to rotate the colour by for the next cell (in degrees)"
  , Arg Help (Just 'h') (Just "help") Nothing "Print this help string"
  ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete langtonArguments
  when (gotArg args Help) $ do
    putStrLn (argsUsage args)
    exitSuccess
  let fps = fromMaybe (error "Couldn't read FPS") (getArg args FPS)
  let skip = fromMaybe (error "Couldn't read skip") (getArg args Skip)
  
  let start = fromMaybe (error "Couldn't read start") (getArg args Start) 
  let time = fromMaybe (error "Couldn't read time") (getArg args Simulations)
  let colour = fromMaybe (error "Couldn't read colour") (getArg args InitialCol)
  let colourShift = fromMaybe (error "Couldn't read colour shift") (getArg args ColShift)
  case getArg args Mode of
    Just "part0" -> do
      let (initialState, firstCell) = newSquareWorld (Ant (Coord 0 0) N (zipWith Transition [0..] [R,L]))
      let world = iterate Ant.walkInACircle initialState
      simulate
         (InWindow "Langton's Ant (Part 0)" (640,480) (100,100))
         black
         fps
         (world !! start)
         (Scale 10 10 . drawWorld colour colourShift)
         (\_ _ -> (!! skip) . iterate Ant.walkInACircle)
 
    Just "square" -> do
      let trans = case getArg args Trans of
            Nothing -> [R,L] -- Langton's ant
            Just xs -> fromMaybe (error "Couldn't read transition system") (readSquareTransition xs)
      let (initialState, firstCell) = newSquareWorld (Ant (Coord 0 0) N (zipWith Transition [0..] trans))
      let world = iterate (Ant.transitionWorld firstCell) initialState
      -- It's a shame that we can't just use world for the next line. If we do, it'll keep too much
      -- in memory as opposed to using constant memory as we'd like :'(
      -- Don't worry too much about this - reasoning about Haskell memory usage is an arcane art
      timeTransitions time (antPosition $ Datastructures.SquareWorld.theAnt $
                             iterate (Ant.transitionWorld firstCell) 
                               initialState !! time)
      simulate
       (InWindow "Langton's Ant (Part 1)" (640,480) (100,100))
       black
       fps
       (world !! start)
       (Scale 10 10 . drawWorld colour colourShift)
       (\_ _ -> (!! skip) . iterate (Ant.transitionWorld firstCell))

    Just "hex" -> do
      let trans = case getArg args Trans of
            Nothing -> [L1,R1] -- Langton's ant
            Just xs -> fromMaybe (error "Couldn't read transition system") (readHexTransition xs) 
      let (initialState, firstCell) = newHexWorld (HexAnt (Coord 0 0) Up (zipWith Transition [0..] trans))
      let world = iterate (Hex.transitionWorld firstCell) initialState
      -- It's a shame that we can't just use world for the next line. If we do, it'll keep too much
      -- in memory as opposed to using constant memory as we'd like :'(
      -- Don't worry too much about this - reasoning about Haskell memory usage is an arcane art
      -- Totally not copied and pasted :p
      timeTransitions time (hexantPosition $ Datastructures.HexWorld.theAnt $
                             iterate (Hex.transitionWorld firstCell)
                               initialState !! time)
      simulate
       (InWindow "Langton's Ant (Part 2)" (640,480) (100,100))
       black
       fps
       (world !! start)
       (Scale 10 10 . drawWorld colour colourShift)
       (\_ _ -> (!! skip) . iterate (Hex.transitionWorld firstCell))
    _ -> putStrLn (argsUsage args)


-- Because no evaluation happens until we need to print the coord
-- this should work. It will however add the time of printing the number
-- to the time taken, however that should be unimportant as long as you
-- use a large number of transitions
timeTransitions :: Int -> Coord -> IO ()
timeTransitions t c = do
  start <- getCurrentTime
  let coord = case c of
        Coord x y -> x `seq` y `seq` Coord x y -- Should evaluate the world
  coord `seq` return ()
  end <- getCurrentTime
  putStr "After "
  putStr $ show t
  putStr " transitions the ant is at "
  putStr (show coord) 
  putStr " and it took us " >> putStr (show $ diffUTCTime end start)
  putStrLn " to calculate that"
