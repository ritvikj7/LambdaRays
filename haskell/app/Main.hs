module Main where

import Lib
import System.IO
import System.Timeout
import System.Console.ANSI

doAnimation :: Vec3 -> IO ()
doAnimation v = do
  clearScreen
  someFunc v
  hFlush stdout -- Immediately print to screen
  move <- timeout 50000 getChar
  case move of
    (Just 'a') -> do doAnimation (v + (Vec3f 0.1 0.0 0.0))
    (Just 'd') -> do doAnimation (v + (Vec3f (-0.1) 0.0 0.0))
    (Just 's') -> do doAnimation (v + (Vec3f 0.0 0.1 0.0))
    (Just 'w') -> do doAnimation (v + (Vec3f 0.0 (-0.1) 0.0))
    _ -> do doAnimation v


main :: IO ()
main = do
  someFunc2 (Vec3f 0.0 0.0 0.0)
  --hSetBuffering stdin NoBuffering                     -- immediately read char
  --hSetBuffering stdout (BlockBuffering (Just 40000))  -- stop flicker
  --hSetEcho stdout False                               -- don't print out input char to stdout
  --doAnimation (Vec3f 0.0 0.0 0.0)
