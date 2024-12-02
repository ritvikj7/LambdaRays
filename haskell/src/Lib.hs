{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Lib
    ( someFunc,
      Vec3(..),
      (*^),
      (*.),
      dot,
      cross,
      vecLength,
      vecDistance,
      vecNormalize,
      SphereRecord(..),  -- Export SphereRecord and all its constructors
      intersect          -- Export the intersect function
    ) where

import Debug.Trace (trace)
import Codec.Picture

someFunc :: Vec3 -> IO () --Vec3 -> IO ()
someFunc = render --putStrLn "someFunc" --v3 = putStrLn (show v3 ++ " " ++ "someFunc")

-- TODO: Add FOV support
aspectRatio :: Float
aspectRatio = 16.0 / 9.0

imgWidth :: Int
imgWidth = 150

imgHeight :: Int
imgHeight = let h = ((fromIntegral imgWidth) / aspectRatio) in
            if h >= 1
            then floor h
            else 1     -- Set height to 1 if h is 0

-- Top left is origin (0,0,0)
imgVecWidth :: Vec3
imgVecWidth = Vec3f (fromIntegral imgWidth) 0.0 0.0

imgVecHeight :: Vec3
imgVecHeight = Vec3f 0.0 ((-1) * fromIntegral imgHeight) 0.0

imgVecX :: Vec3
imgVecX = vecNormalize imgVecWidth

imgVecY :: Vec3
imgVecY = vecNormalize imgVecHeight

viewHeight :: Float
viewHeight = 2.0

viewWidth :: Float
viewWidth = viewHeight * aspectRatio

imgViewH :: Vec3
imgViewH = Vec3f 0.0 (-viewHeight) 0.0

imgViewW :: Vec3
imgViewW = Vec3f (viewWidth) 0.0 0.0

imgViewX :: Vec3
imgViewX = (1.0 / (fromIntegral imgWidth)) *^ imgViewW

imgViewY :: Vec3
imgViewY = (1.0 / (fromIntegral imgHeight)) *^ imgViewH

near :: Float
near = 1.0 -- Distance to near plane

focalLen :: Float
focalLen = 1.0 --(sqrt ((fromIntegral imgWidth)^2 + (fromIntegral imgHeight)^2)) / ( 2*(tan (pi / 8)  ) )

cameraNormal :: Vec3
cameraNormal = (Vec3f 0.0 0.0 ((-focalLen) * near)) -- vecNormalize ( 0 0 -near) -- assume right hand coordinate system. -z is forward, y is up, x is right

cameraPos :: Vec3
cameraPos = Vec3f 0.0 0.0 0.0 -- camera position is origin of this view frame

imgOriginPixelEdge :: Vec3
imgOriginPixelEdge = cameraPos + cameraNormal + ((-0.5) *^ imgViewW) + ((-0.5) *^ imgViewH) -- THIS IS CAMERA coordinates (not screen coordinates!)

imgOriginPixelCenter :: Vec3
imgOriginPixelCenter = imgOriginPixelEdge + (0.5 *^ imgViewX) + (0.5 *^ imgViewY)

generateZeroedPixelArr :: Int -> Int -> [Char]
generateZeroedPixelArr w h = [' ' | _ <- [1..(w*h)]]

generateZeroedFrameBuffer :: Int -> Int -> [Vec3]
generateZeroedFrameBuffer w h = [Vec3f 0.0 0.0 0.0 | _ <- [1..(w*h)]]

pixelArr :: [Char]
pixelArr = generateZeroedPixelArr imgWidth imgHeight

framebuffer :: [Vec3]
framebuffer = generateZeroedFrameBuffer imgWidth imgHeight


-- Throwaway
convertRGBtoChar :: Vec3 -> Char
convertRGBtoChar (Vec3f r g b) | (nearlyEqual r g 0.1) && (nearlyEqual g b 0.1) = 'W'
                               | r > g && r > b   = 'R'
                               | g > r && g > b   = 'G'
                               | otherwise        = 'B'

createRay :: (Int, Int) -> Vec3 -> Vec3
createRay (x, y) o = (imgOriginPixelCenter + (fromIntegral x *^ imgViewX) + (fromIntegral y *^ imgViewY)) - o

drawBackground :: (Int, Int) -> Char
drawBackground (_, y) = convertRGBtoChar (vecLerp (Vec3f 0.5 0.7 1.0) (Vec3f 1.0 1.0 1.0) ((fromIntegral y) / (fromIntegral imgHeight)))


drawPixel :: (Int, Int) -> Vec3 -> Char
drawPixel (x, y) v = let intersectedRecord = (intersectPixel (x, y) v) in
                     if (snd intersectedRecord >= 0.0)
                     then convertRGBtoChar (computeNormal intersectedRecord (createRay (x,y) cameraPos) cameraPos) --'*'
                     else drawBackground (x, y)


intersectPixel :: (Int, Int) -> Vec3 -> (SphereRecord, Float)
intersectPixel (x, y) v = intersectAll ((imgOriginPixelCenter + (fromIntegral x *^ imgViewX) + (fromIntegral y *^ imgViewY)) - v) listOfSpheres v
      --then '*' --print ("YES " ++ (show (imgOriginPixelCenter + (fromIntegral (i `mod` imgWidth) *^ imgViewX) + (fromIntegral (i `div` imgWidth) *^ imgViewY))) ++ " ") --'*'
      --else ' ' --print ("NO " ++ (show (imgOriginPixelCenter + (fromIntegral (i `mod` imgWidth) *^ imgViewX) + (fromIntegral (i `div` imgWidth) *^ imgViewY))) ++ " ")

getImgCoords :: Int -> (Int, Int)
getImgCoords i = (i `mod` imgWidth, i `div` imgWidth)

mapPixels :: Vec3 -> [Char]
mapPixels v = map (\(i, _) -> (drawPixel (getImgCoords i) v)) (zip [0..] pixelArr)


insertNewLines :: Int -> [Char] -> [Char]
insertNewLines _ [] = []
insertNewLines i (pixel:pixels) = if ((i `mod` imgWidth) == 0) -- (i /= 0)
                                then '\n':pixel:(insertNewLines (i+1) pixels)
                                else pixel:(insertNewLines (i+1) pixels)

render :: Vec3 -> IO ()
render v = putStrLn (insertNewLines 0 (mapPixels v))

-- Use Record syntax
data SphereRecord = Sphere { position :: Vec3, radius :: Float }
  deriving (Show, Eq)

listOfSpheres :: [SphereRecord]
listOfSpheres = [Sphere { position = Vec3f 0.0 0.0 (-focalLen - 2), radius = 0.5}, Sphere { position = Vec3f 2.0 0.0 (-focalLen - 2), radius = 0.25}]

nullSphere :: SphereRecord
nullSphere = Sphere { position = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF , radius = 0xDEADBEEF} -- Debug values

testSphere :: SphereRecord
testSphere = Sphere { position = Vec3f 0.0 0.0 (-focalLen - 2), radius = 0.5}

closestOrInvalid :: [(SphereRecord, Float)] -> (SphereRecord, Float)
closestOrInvalid [] = (nullSphere, -1.0)
closestOrInvalid (x:xs) = foldr minT x xs
                        where minT tuple1 tuple2 = if snd tuple1 < snd tuple2
                                                   then tuple1
                                                   else tuple2

intersectAll :: Vec3 -> [SphereRecord] -> Vec3 -> (SphereRecord, Float)
intersectAll rayDir spheres camPos = closestOrInvalid (filter (\(_, t) -> t >= 0.0) (map (\(sphere) -> (sphere, intersect rayDir sphere camPos)) spheres))

intersect :: Vec3 -> SphereRecord -> Vec3 -> Float
intersect rayDir sphere camPos = if d >= 0
                            then ((-b) - (sqrt d)) / (2*vdotv)
                            else -1.0
                            where oc = (camPos - (position sphere))
                                  v = rayDir
                                  vdotv = v `dot` v
                                  r = (radius sphere)
                                  b = 2 * (v `dot` oc)
                                  d = b^2 - 4*vdotv*((oc `dot` oc) - r*r)

-- Compute normalized normal from sphere intersection point to sphere center
computeNormal :: (SphereRecord, Float) -> Vec3 -> Vec3 -> Vec3
computeNormal (sphere, t) ray camPos = vecNormalize (((t *^ ray) + camPos) - (position sphere)) -- (1.0 / (radius sphere)) *^ (((t *^ ray) + camPos) - (position sphere)) -- NOTE: cameraPos is (0,0,0) not updated camPos after controls movement

-- Vec3 ADT; could use type punning but not a fan so I didn't
data Vec3 = Vec3f Float Float Float
  deriving (Show, Eq)

-- Implement Vec3 as a instance of Num type class to get conveient vector algebra functions and utils
instance Num Vec3 where
    (+) :: Vec3 -> Vec3 -> Vec3
    (Vec3f x1 y1 z1) + (Vec3f x2 y2 z2) = Vec3f (x1 + x2) (y1 + y2) (z1 + z2)

    (-) :: Vec3 -> Vec3 -> Vec3
    (Vec3f x1 y1 z1) - (Vec3f x2 y2 z2) = Vec3f (x1 - x2) (y1 - y2) (z1 - z2)

    negate :: Vec3 -> Vec3
    negate (Vec3f x y z) = Vec3f (-x) (-y) (-z)

    fromInteger :: Integer -> Vec3
    fromInteger n = Vec3f (fromIntegral n) (fromIntegral n) (fromIntegral n)

    abs :: Vec3 -> Vec3
    abs (Vec3f x y z) = Vec3f (abs x) (abs y) (abs z)

    -- Signum will return the direction of the vector (a normalized vector)
    signum :: Vec3 -> Vec3
    signum = vecNormalize

    (*) = error "Vector multiplication is errorneous: use dot or cross product instead"

-- Scalar multiplication on both ends
(*^) :: Float -> Vec3 -> Vec3
scalar *^ (Vec3f x y z) = Vec3f (scalar*x) (scalar*y) (scalar*z)

(*.) :: Vec3 -> Float -> Vec3
(Vec3f x y z) *. scalar = Vec3f (scalar*x) (scalar*y) (scalar*z)

-- Dot product
dot :: Vec3 -> Vec3 -> Float
dot (Vec3f x1 y1 z1) (Vec3f x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Cross product
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3f x1 y1 z1) (Vec3f x2 y2 z2) =
    Vec3f (y1 * z2 - z1 * y2)
          (z1 * x2 - x1 * z2)
          (x1 * y2 - y1 * x2)


vecDistance :: Vec3 -> Vec3 -> Float
vecDistance v1 v2 = sqrt (diff `dot` diff)
                  where diff = v1 - v2

vecLength :: Vec3 -> Float
vecLength v = sqrt (v `dot` v)

vecNormalize :: Vec3 -> Vec3
vecNormalize (Vec3f x y z) = if norm /= 0.0
                             then (1.0 / norm) *^ v
                             else Vec3f 0.0 0.0 0.0
                          where v = Vec3f x y z
                                norm = vecLength v

floatLerp :: Float -> Float -> Float -> Float
floatLerp a b t = (1.0 - t) * a + t * b

vecLerp :: Vec3 -> Vec3 -> Float -> Vec3
vecLerp a b t = (1.0 - t) *^ a + t *^ b
              -- where t' = trace ("Lerp t value: " ++ show t) t

nearlyEqual :: Float -> Float -> Float -> Bool
nearlyEqual a b epsilon = abs (a - b) < epsilon

vec3ToPixel :: Vec3 -> PixelRGB8
vec3ToPixel (Vec3f r g b) = PixelRGB8 (floor (r * 255)) (floor (g * 255)) (floor (b * 255))

writeVec3Image :: FilePath -> [Vec3] -> IO ()
writeVec3Image filePath frameBuffer = writePng filePath (generateImage pixelRenderer imgWidth imgHeight)
                                      where pixelRenderer x y = vec3ToPixel (frameBuffer !! (y * imgWidth + x)) -- Need to replace with a vector so we get O(1) indexing instead of O(n) LL
