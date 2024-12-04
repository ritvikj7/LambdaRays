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

{- Generate an initial framebuffer filled with zeroed color vectors
Each pixel in the framebuffer is initialized to (0.0, 0.0, 0.0), representing black -}
generateZeroedFrameBuffer :: Int -> Int -> [Vec3]
generateZeroedFrameBuffer w h = [Vec3f 0.0 0.0 0.0 | _ <- [1..(w*h)]]

{- Initialize the framebuffer for the entire image
`imgWidth` and `imgHeight` are the image's dimensions defined elsewhere -}
framebuffer :: [Vec3]
framebuffer = generateZeroedFrameBuffer imgWidth imgHeight

{- Compute the color of a single pixel at (x, y)
This includes sampling the pixel multiple times (`spp` samples per pixel) for anti-aliasing -}
drawPixel :: (Int, Int) -> Vec3
drawPixel (x, y) = gammaCorrect ((1.0 / (fromIntegral spp)) *^ samplePixel(x, y) spp)

{- Compute the colors for all pixels in the image
Each pixel is processed by mapping `drawPixel` over the coordinates in the framebuffer -}
drawPixels :: [Vec3]
drawPixels = map (\(i, _) -> (drawPixel (getImgCoords i))) (zip [0..] framebuffer)

render :: IO ()
render =  writeVec3Image "output.png" drawPixels
