{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Lib
    ( someFunc,
    Vec3(..)
    ) where

someFunc :: Vec3 -> IO () --Vec3 -> IO ()
someFunc = render --putStrLn "someFunc" --v3 = putStrLn (show v3 ++ " " ++ "someFunc")

-- TODO: Add FOV support
aspectRatio :: Float
aspectRatio = 16.0 / 9.0

imgWidth :: Int
imgWidth = 64

imgHeight :: Int
imgHeight = floor (fromIntegral imgWidth / aspectRatio) -- Should probably check if number is floored to 0

-- Top left is origin (0,0,0)
imgVecWidth :: Vec3
imgVecWidth = Vec3f (fromIntegral imgWidth) 0.0 0.0

imgVecHeight :: Vec3
imgVecHeight = Vec3f 0.0 ((-1) * fromIntegral imgHeight) 0.0

imgVecX :: Vec3
imgVecX = vecNormalize imgVecWidth

imgVecY :: Vec3
imgVecY = vecNormalize imgVecHeight

imgViewH :: Vec3
imgViewH = Vec3f 0.0 (-2.0) 0.0

imgViewW :: Vec3
imgViewW = Vec3f (2.0 * aspectRatio) 0.0 0.0

imgViewX :: Vec3
imgViewX = (1.0 / (fromIntegral imgWidth)) *^ imgViewW

imgViewY :: Vec3
imgViewY = (1.0 / (fromIntegral imgHeight)) *^ imgViewH

near :: Float
near = 1.0 -- Distance to near plane

focalLen :: Float
focalLen = (sqrt ((fromIntegral imgWidth)^2 + (fromIntegral imgHeight)^2)) / ( 2*(tan (pi / 8)  ) )

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

pixelArr = generateZeroedPixelArr imgWidth imgHeight

drawPixel :: Int -> Vec3 -> Char --IO () --Char
drawPixel i v = if (intersect ((imgOriginPixelCenter - v) + (fromIntegral (i `mod` imgWidth) *^ imgViewX) + (fromIntegral (i `div` imgWidth) *^ imgViewY)) sphere1 v)
      then '*' --print ("YES " ++ (show (imgOriginPixelCenter + (fromIntegral (i `mod` imgWidth) *^ imgViewX) + (fromIntegral (i `div` imgWidth) *^ imgViewY))) ++ " ") --'*'
      else ' ' --print ("NO " ++ (show (imgOriginPixelCenter + (fromIntegral (i `mod` imgWidth) *^ imgViewX) + (fromIntegral (i `div` imgWidth) *^ imgViewY))) ++ " ")

mapPixels :: Vec3 -> [Char]
mapPixels v = map (\(i, _) -> (drawPixel i v)) (zip [0..] pixelArr)


insertNewLines :: Int -> [Char] -> [Char]
insertNewLines _ [] = []
insertNewLines i (pixel:pixels) = if ((i `mod` imgWidth) == 0) -- (i /= 0)
                                then '\n':pixel:(insertNewLines (i+1) pixels)
                                else pixel:(insertNewLines (i+1) pixels)

render :: Vec3 -> IO ()
render v = putStrLn (insertNewLines 0 (mapPixels v))

sphere1 :: SphereRecord
sphere1 = Sphere { position = Vec3f 0.0 0.0 ((-focalLen) + 10.0), radius = 0.5}

-- Use Record syntax
data SphereRecord = Sphere { position :: Vec3, radius :: Float }
  deriving (Show, Eq)

intersect :: Vec3 -> SphereRecord -> Vec3 -> Bool
intersect v sphere camPos = (d >= 0) && ((-b) - (sqrt d)) / (2*vdotv) >= 0
                      where oc = (camPos - (position sphere))
                            vdotv = v `dot` v
                            r = (radius sphere)
                            b = 2 * (v `dot` oc)
                            d = b^2 - 4*vdotv*((oc `dot` oc) - r*r)

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

vecDistance :: Vec3 -> Vec3 -> Float
vecDistance v1 v2 = sqrt (v1 `dot` v2)

vecLength :: Vec3 -> Float
vecLength v = sqrt (v `dot` v)

vecNormalize :: Vec3 -> Vec3
vecNormalize (Vec3f x y z) = if norm /= 0.0
                             then (1.0 / norm) *^ v
                             else Vec3f 0.0 0.0 0.0
                          where v = Vec3f x y z
                                norm = vecLength v

