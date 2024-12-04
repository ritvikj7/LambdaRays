
module Utils where

import Vec3
import Camera

import Codec.Picture
import System.Random
import System.IO.Unsafe (unsafePerformIO)

-- Math Utils --

floatLerp :: Float -> Float -> Float -> Float
floatLerp a b t = (1.0 - t) * a + t * b

vecLerp :: Vec3 -> Vec3 -> Float -> Vec3
vecLerp a b t = (1.0 - t) *^ a + t *^ b
              -- where t' = trace ("Lerp t value: " ++ show t) t

nearlyEqual :: Float -> Float -> Float -> Bool
nearlyEqual a b epsilon = abs (a - b) < epsilon

-- Image processing --

gammaCorrect :: Vec3 -> Vec3
gammaCorrect (Vec3f r g b) = Vec3f (r ** (1.0 / 2.2)) (g ** (1.0 / 2.2))  (b ** (1.0 / 2.2)) -- Use gamma exponent 2.2

vec3ToPixel :: Vec3 -> PixelRGB8
vec3ToPixel (Vec3f r g b) = PixelRGB8 (floor (r * 255.9)) (floor (g * 255.9)) (floor (b * 255.9))

-- Credit to JuicyPixel documentation for this function: https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture.html#v:generateImage
writeVec3Image :: FilePath -> [Vec3] -> IO ()
writeVec3Image filePath frameBuffer = writePng filePath (generateImage pixelRenderer imgWidth imgHeight)
                                      where pixelRenderer x y = vec3ToPixel (frameBuffer !! (y * imgWidth + x)) -- Need to replace with a vector so we get O(1) indexing instead of O(n) LL


-- Misc --
-- Credit to chatGPT for this unpure RNG function
generateRandomNum :: Float -> Float -> IO Float
generateRandomNum min max = do
    gen <- getStdGen
    let (randomNum, gen1) = randomR (min, max) gen
    setStdGen gen1 -- Update the global generator
    return randomNum
