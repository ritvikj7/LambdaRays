{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Lib
    ( --someFunc,
      someFunc2,
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
import qualified Data.Vector as V
import System.Random
import System.IO.Unsafe (unsafePerformIO)

--someFunc :: Vec3 -> IO () --Vec3 -> IO ()
--someFunc = renderStringFB --putStrLn "someFunc" --v3 = putStrLn (show v3 ++ " " ++ "someFunc")

someFunc2 :: IO ()
someFunc2 = render

-- Sampling information --
spp :: Int
spp = 5

-- Camera data --

-- TODO: Add FOV support
aspectRatio :: Float
aspectRatio = 16.0 / 9.0

imgWidth :: Int
imgWidth = 400

imgHeight :: Int
imgHeight = let h = ((fromIntegral imgWidth) / aspectRatio) in
            if h >= 1
            then floor h
            else 1     -- Set height to 1 if h is 0

-- Top left is origin (0,0,0)
imgVecWidth :: Vec3
imgVecWidth = Vec3f (fromIntegral imgWidth) 0.0 0.0

imgVecHeight :: Vec3
imgVecHeight = Vec3f 0.0 ((-1.0) * fromIntegral imgHeight) 0.0

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
near = 1.0 -- TODO: Using focalLen instead; get rid of this

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

computeImgOriginPixelCenter :: Vec3 -> Vec3
computeImgOriginPixelCenter newCamPos = (newCamPos + cameraNormal + ((-0.5) *^ imgViewW) + ((-0.5) *^ imgViewH)) + (0.5 *^ imgViewX) + (0.5 *^ imgViewY)


-- Ray functions and coords --

getRayAtT :: Vec3 -> Float -> Vec3 -> Vec3
getRayAtT o t v = o + (t *^ v)

-- TODO: Fix this name, should be createViewportRay ?
createRay :: (Float, Float) -> Vec3
createRay (x, y) = (imgOriginPixelCenter + (x *^ imgViewX) + (y *^ imgViewY)) - cameraPos -- Replace 'cameraPos' with 'o'

getImgCoords :: Int -> (Int, Int)
getImgCoords i = (i `mod` imgWidth, i `div` imgWidth)


-- Vec3 framebuffer functions -- TODO: Switch out linked lists for Data.Vector

generateZeroedFrameBuffer :: Int -> Int -> [Vec3]
generateZeroedFrameBuffer w h = [Vec3f 0.0 0.0 0.0 | _ <- [1..(w*h)]]

framebuffer :: [Vec3]
framebuffer = generateZeroedFrameBuffer imgWidth imgHeight

drawBackground :: (Float, Float) -> Vec3
drawBackground (_, y) = vecLerp (Vec3f 0.5 0.7 1.0) (Vec3f 1.0 1.0 1.0) (y / (fromIntegral imgHeight)) -- (Vec3f 0.005 0.005 0.02) (Vec3f 0.0 0.0 0.0)

data LightRecord = Light { posLight :: Vec3, normalLight :: Vec3, colorLight :: Vec3 }
  deriving (Show, Eq)

light1 :: LightRecord
light1 = Light { posLight = Vec3f (-1.0) 1.5 (0.25) , normalLight = Vec3f (-1.0) 0.0 0.0, colorLight = Vec3f 0.3 0.3 0.3}

kA :: Float
kA = 0.005

kD :: Float
kD = 0.8

-- Swapped out for per-sphere albedo
--diffuseColor :: Vec3
--diffuseColor = Vec3f 0.5 0.0 0.0

kS :: Float
kS = 0.5

-- Swapped out for per-sphere albedo
--ambientColor :: Vec3
--ambientColor = Vec3f 0.025 0.0 0.15


shadeSphere :: (Float, Float) -> Vec3 -> (Vec3, SphereRecord, Vec3) -> Int -> Vec3
shadeSphere (x,y) incidentRay (normal, sphere, hitPoint) depth = let shadowRay = vecNormalize ((posLight light1) - hitPoint)
                                                                     intersectRecord = (intersectAll shadowRay hitPoint (filter (/= sphere) listOfSpheres)) -- Got rid of shadow acne with this, let's goooo!
                                                                     sphereMaterial = (material sphere)
                                                                     sphereAlbedo = (albedo sphere)
                                                                     tShadow = (snd intersectRecord)
                                                                  in if (nearlyEqual tShadow (-1.0) 0.001) -- Self intersections will cause to set off sometimes (shadow acne)
                                                                     then if (sphereMaterial == NonMetal)
                                                                     then shadeNonMetalSphere (normal, sphere, hitPoint)
                                                                     else shadeReflectiveSphere (x,y) incidentRay (normal, sphere, hitPoint) depth
                                                                     else kA *^ sphereAlbedo

shadeReflectiveSphere :: (Float, Float) -> Vec3 -> (Vec3, SphereRecord, Vec3) -> Int -> Vec3
shadeReflectiveSphere (x,y) incidentRay (normal, sphere, hitPoint) 0 = (albedo sphere)
shadeReflectiveSphere (x,y) incidentRay (normal, sphere, hitPoint) depth = let reflectRay = vecNormalize (vecReflect (vecNormalize incidentRay) normal) -- TODO: Gotta clean up these normalizations. I'm throwing them out like free candy lol
                                                                               (Vec3f albedoR albedoG albedoB) = (albedo sphere)
                                                                               intersectRecord = (intersectAll reflectRay hitPoint (filter (/= sphere) listOfSpheres)) -- Need to be aware of shadow acne issue here with bounce ray
                                                                               newSphere = (fst intersectRecord)
                                                                               tReflect = (snd intersectRecord)
                                                                            in if tReflect >= 0.0
                                                                               then let newNormal = computeNormal intersectRecord reflectRay hitPoint
                                                                                        newHitPoint = getRayAtT hitPoint tReflect reflectRay
                                                                                        (Vec3f cx cy cz) = shadeSphere (x, y) incidentRay (newNormal, newSphere, newHitPoint) (depth - 1)
                                                                                    in (Vec3f (albedoR * cx) (albedoG * cy) (albedoB * cz))
                                                                               else let (Vec3f bx by bz) = drawBackground (x,y)
                                                                                    in (Vec3f (albedoR * bx) (albedoG * by) (albedoB * bz))

shadeNonMetalSphere :: (Vec3, SphereRecord, Vec3) -> Vec3
shadeNonMetalSphere (normal, sphere, hitPoint) = let shadowRay = vecNormalize ((posLight light1) - hitPoint)
                                                     view = vecNormalize (cameraPos - hitPoint)
                                                     half = vecNormalize (shadowRay + view)
                                                     lightColor = (colorLight light1)
                                                     sphereAlbedo = (albedo sphere)

                                                     ambient = kA *^ sphereAlbedo
                                                     diffuse = (kD * (max (dot normal shadowRay) 0.0)) *^ sphereAlbedo
                                                     specular = (kS * ((max (dot normal half) 0.0) ** 32.0  )) *^ lightColor -- Turned off specular because I have some bug? Maybe I'm computing the half vector wrong for Blinn-Phong

                                                     (Vec3f cx cy cz) = ambient + diffuse + specular
                                                 in (Vec3f (min cx 1.0) (min cy 1.0) (min cz 1.0))

samplePixel :: (Int, Int) -> Int -> Vec3
samplePixel _ 0 = Vec3f 0.0 0.0 0.0
samplePixel (x, y) numSamples = let (offsetX, offsetY) = generatePixelOffset (fromIntegral x, fromIntegral y)
                                    intersectedSphereRecord = (intersectPixel (offsetX, offsetY) listOfSpheres)
                                    sphere = (fst intersectedSphereRecord)
                                    tSphere = (snd intersectedSphereRecord)
                                    ray = createRay (offsetX, offsetY)
                                  in if (tSphere >= 0.0)
                                     then let (Vec3f nx ny nz) = (computeNormal intersectedSphereRecord ray cameraPos)
                                              hitPoint = getRayAtT cameraPos tSphere ray
                                              rayDepth = 5
                                          in shadeSphere (offsetX, offsetY) ray ((Vec3f nx ny nz), sphere, hitPoint) rayDepth + (samplePixel (x, y) (numSamples - 1)) --(0.5 *^ (Vec3f (nx + 1.0) (ny + 1.0) (nz + 1.0))) to visualize normals
                                     else drawBackground (offsetX, offsetY) + (samplePixel (x, y) (numSamples - 1))

drawPixel :: (Int, Int) -> Vec3
drawPixel (x, y) = gammaCorrect ((1.0 / (fromIntegral spp)) *^ samplePixel(x, y) spp)

mapPixels :: [Vec3]
mapPixels = map (\(i, _) -> (drawPixel (getImgCoords i))) (zip [0..] framebuffer)

render :: IO ()
render =  writeVec3Image "output.png" (mapPixels)

-- String framebuffer functions --
{--
generateZeroedStringFB :: Int -> Int -> [Char]
generateZeroedStringFB w h = [' ' | _ <- [1..(w*h)]]

stringFB :: [Char]
stringFB = generateZeroedStringFB imgWidth imgHeight


convertRGBtoChar :: Vec3 -> Char
convertRGBtoChar (Vec3f r g b) | (nearlyEqual r g 0.1) && (nearlyEqual g b 0.1) = 'W'
                               | r > g && r > b   = 'R'
                               | g > r && g > b   = 'G'
                               | otherwise        = 'B'

drawBackgroundStringFB :: (Int, Int) -> Char
drawBackgroundStringFB (_, y) = convertRGBtoChar (vecLerp (Vec3f 0.5 0.7 1.0) (Vec3f 1.0 1.0 1.0) ((fromIntegral y) / (fromIntegral imgHeight)))


drawPixelStringFB :: (Int, Int) -> Vec3 -> Char
drawPixelStringFB (x, y) o = let intersectedRecord = (intersectPixel (fromIntegral x, fromIntegral y)) in
                     if (snd intersectedRecord >= 0.0)
                     then convertRGBtoChar (computeNormal intersectedRecord (createRay (fromIntegral x, fromIntegral y))) --'*'
                     else drawBackgroundStringFB (x, y)


mapPixelsStringFB :: Vec3 -> [Char]
mapPixelsStringFB o = map (\(i, _) -> (drawPixelStringFB (getImgCoords i) o)) (zip [0..] stringFB)


insertNewLinesStringFB :: Int -> [Char] -> [Char]
insertNewLinesStringFB _ [] = []
insertNewLinesStringFB i (pixel:pixels) = if ((i `mod` imgWidth) == 0) -- (i /= 0)
                                then '\n':pixel:(insertNewLinesStringFB (i+1) pixels)
                                else pixel:(insertNewLinesStringFB (i+1) pixels)

renderStringFB :: Vec3 -> IO ()
renderStringFB o = putStrLn (insertNewLinesStringFB 0 (mapPixelsStringFB o))
--}

-- Intersect functions --

closestOrInvalid :: (SceneObject a) => [(a, Float)] -> (a, Float)
closestOrInvalid [] = (nullObject, -1.0)
closestOrInvalid (x:xs) = foldr minT x xs
                          where minT tuple1 tuple2 = if snd tuple1 < snd tuple2
                                                     then tuple1
                                                     else tuple2

intersectPixel :: (SceneObject a) => (Float, Float) -> [a] -> (a, Float)
intersectPixel (x, y) = intersectAll (createRay (x,y)) cameraPos

intersectAll :: (SceneObject a) => Vec3 -> Vec3 -> [a] -> (a, Float)
intersectAll rayDir rayOrigin sceneObjs = closestOrInvalid (filter (\(_, t) -> t >= 0.0) (map (\(sceneObj) -> (sceneObj, intersect rayDir rayOrigin sceneObj)) sceneObjs))

instance SceneObject SphereRecord where
  intersect :: Vec3 -> Vec3 -> SphereRecord -> Float
  intersect rayDir rayOrigin sphere = if d >= 0
                            then ((-b) - (sqrt d)) / (2*vdotv)
                            else -1.0
                            where oc = (rayOrigin - (position sphere))
                                  v = rayDir
                                  vdotv = v `dot` v
                                  r = (radius sphere)
                                  b = 2 * (v `dot` oc)
                                  d = b^2 - 4*vdotv*((oc `dot` oc) - r*r)
  nullObject :: SphereRecord
  nullObject = nullSphere

instance SceneObject PlaneRecord where
  intersect :: Vec3 -> Vec3 -> PlaneRecord -> Float
  intersect rayDir rayOrigin plane = error "TODO: Finish this"

  nullObject :: PlaneRecord
  nullObject = nullPlane

-- Compute normalized normal from sphere intersection point to sphere center
computeNormal :: (SphereRecord, Float) -> Vec3 -> Vec3 -> Vec3
computeNormal (sphere, t) ray o = if t >= 0.0
                                  then  (1.0 / (radius sphere)) *^ (((t *^ ray) + o) - (position sphere))
                                  else (Vec3f 0.0 0.0 0.0)

-- Credit to chatGPT for this unpure RNG function
generateRandomNum :: Float -> Float -> IO Float
generateRandomNum min max = do
    gen <- getStdGen
    let (randomNum, gen1) = randomR (min, max) gen
    setStdGen gen1 -- Update the global generator
    return randomNum


generatePixelOffset :: (Float, Float) -> (Float, Float)
generatePixelOffset (x, y) = let offsetX = unsafePerformIO $ generateRandomNum (-0.5) 0.5
                                 offsetY = unsafePerformIO $ generateRandomNum (-0.5) 0.5
                             in (x + offsetX, y + offsetY)

-- Scene Object ---
class SceneObject a where
  intersect :: Vec3 -> Vec3 -> a -> Float
  nullObject :: a

-- Plane object ---
data PlaneRecord = Plane { pointPlane :: Vec3, normalPlane :: Vec3 }
  deriving (Show, Eq)

listOfPlanes :: [PlaneRecord]
listOfPlanes = [Plane { pointPlane = Vec3f (-2.5) 0.0 (-focalLen - 5), normalPlane = (vecNormalize (Vec3f (1.0) 0.0 0.0))}]

nullPlane :: PlaneRecord
nullPlane = Plane { pointPlane = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF, normalPlane = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF }

-- Material ADT --
data Material = Metal | NonMetal
  deriving (Show, Eq)

-- Sphere data and functions --
data SphereRecord = Sphere { position :: Vec3, radius :: Float, material :: Material, albedo :: Vec3}
  deriving (Show, Eq)

listOfSpheres :: [SphereRecord]
listOfSpheres = [Sphere { position = Vec3f 0.0 0.0 (-focalLen - 1), radius = 0.5, material = NonMetal, albedo = Vec3f 1.0 0.0 0.05}, Sphere { position = Vec3f 1.25 0.1 (-focalLen - 1), radius = 0.35, material = NonMetal, albedo = (Vec3f 0.2 0.8 0.2)},
                 Sphere { position = Vec3f (-1.5) 0.0 (-focalLen - 1), radius = 0.6, material = Metal, albedo = (Vec3f 0.8 0.8 0.8)}, Sphere { position = Vec3f 0.0 (-100.5) (-focalLen), radius = 100, material = NonMetal, albedo = Vec3f 0.8 0.8 0.0 },
                 Sphere { position = Vec3f 1.0 0.5 0.4, radius = 0.8, material = NonMetal, albedo = (Vec3f 0.0 0.1 0.9)}, Sphere {position = Vec3f (0.0) 1.25 (-focalLen - 1.075), radius = 0.5, material = Metal, albedo = (Vec3f 0.8 0.8 0.8)}]

nullSphere :: SphereRecord
nullSphere = Sphere { position = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF , radius = 0xDEADBEEF, material = NonMetal, albedo = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF} -- Debug values

testSphere :: SphereRecord
testSphere = Sphere { position = Vec3f 0.0 0.0 (-focalLen - 2), radius = 0.5, material = NonMetal, albedo = Vec3f 1.0 0.0 0.0}



-- Vec3 functions --


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

vecLength2 :: Vec3 -> Float
vecLength2 v = v `dot` v

vecNormalize :: Vec3 -> Vec3
vecNormalize (Vec3f x y z) = if norm /= 0.0
                             then (1.0 / norm) *^ v
                             else Vec3f 0.0 0.0 0.0
                          where v = Vec3f x y z
                                norm = vecLength v

vecReflect :: Vec3 -> Vec3 -> Vec3
vecReflect v n = v - ((2.0 * dot v n) *^ n)

-- Camera and ray data and functions --

-- Utils --
floatLerp :: Float -> Float -> Float -> Float
floatLerp a b t = (1.0 - t) * a + t * b

vecLerp :: Vec3 -> Vec3 -> Float -> Vec3
vecLerp a b t = (1.0 - t) *^ a + t *^ b
              -- where t' = trace ("Lerp t value: " ++ show t) t

nearlyEqual :: Float -> Float -> Float -> Bool
nearlyEqual a b epsilon = abs (a - b) < epsilon

-- Image processing --

gammaCorrect :: Vec3 -> Vec3
gammaCorrect (Vec3f r g b) = Vec3f (r ** (1.0 / 2.0)) (g ** (1.0 / 2.0))  (b ** (1.0 / 2.0)) -- Use gamma exponent 2.2

vec3ToPixel :: Vec3 -> PixelRGB8
vec3ToPixel (Vec3f r g b) = PixelRGB8 (floor (r * 255.9)) (floor (g * 255.9)) (floor (b * 255.9))

writeVec3Image :: FilePath -> [Vec3] -> IO ()
writeVec3Image filePath frameBuffer = writePng filePath (generateImage pixelRenderer imgWidth imgHeight)
                                      where pixelRenderer x y = vec3ToPixel (frameBuffer !! (y * imgWidth + x)) -- Need to replace with a vector so we get O(1) indexing instead of O(n) LL
