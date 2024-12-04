{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Renderer where

import Vec3
import Utils
import Camera
import ScenePrimitives
import PixelShader

import Debug.Trace (trace)
import Codec.Picture
import qualified Data.Vector as V
import System.Random
import System.IO.Unsafe (unsafePerformIO)

--renderEntryStringFB :: Vec3 -> IO () --Vec3 -> IO ()
--renderEntryStringFB = renderStringFB --putStrLn "someFunc" --v3 = putStrLn (show v3 ++ " " ++ "someFunc")

renderEntry :: IO ()
renderEntry = render

-- Vec3 framebuffer functions -- TODO: Switch out linked lists for Data.Vector

generateZeroedFrameBuffer :: Int -> Int -> [Vec3]
generateZeroedFrameBuffer w h = [Vec3f 0.0 0.0 0.0 | _ <- [1..(w*h)]]

framebuffer :: [Vec3]
framebuffer = generateZeroedFrameBuffer imgWidth imgHeight

drawPixel :: (Int, Int) -> Vec3
drawPixel (x, y) = gammaCorrect ((1.0 / (fromIntegral spp)) *^ samplePixel(x, y) spp)

drawPixels :: [Vec3]
drawPixels = map (\(i, _) -> (drawPixel (getImgCoords i))) (zip [0..] framebuffer)

render :: IO ()
render =  writeVec3Image "output.png" drawPixels
