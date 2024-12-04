
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module ScenePrimitives where
import Vec3
import Camera

-- Scene Object ---
class SceneObject a where
  intersect :: Vec3 -> Vec3 -> a -> Float
  nullObject :: a

-- Sphere data and functions --
data SphereRecord = Sphere { position :: Vec3, radius :: Float, material :: Material, albedo :: Vec3} -- TODO: Due to Naming collisions, we should change position, radius, etc. to posSphere, radiusSphere, etc.
  deriving (Show, Eq)

listOfSpheres :: [SphereRecord]
listOfSpheres = [Sphere { position = Vec3f 0.0 0.0 (-focalLen - 1), radius = 0.5, material = NonMetal, albedo = Vec3f 1.0 0.0 0.05},
                 Sphere { position = Vec3f 1.25 0.1 (-focalLen - 1), radius = 0.35, material = NonMetal, albedo = (Vec3f 0.2 0.8 0.2)},
                 Sphere { position = Vec3f (-1.5) 0.0 (-focalLen - 1), radius = 0.6, material = Metal, albedo = (Vec3f 0.8 0.8 0.8)},
                 Sphere { position = Vec3f 0.0 (-100.5) (-focalLen), radius = 100, material = NonMetal, albedo = Vec3f 0.8 0.8 0.0 },
                 Sphere { position = Vec3f 1.0 0.5 0.4, radius = 0.8, material = NonMetal, albedo = (Vec3f 0.0 0.1 0.9)}] --,Sphere {position = Vec3f (0.0) 1.25 (-focalLen - 1.075), radius = 0.5, material = Metal, albedo = (Vec3f 0.8 0.8 0.8)}] This sphere adds top reflective sphere

nullSphere :: SphereRecord
nullSphere = Sphere { position = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF , radius = 0xDEADBEEF, material = NonMetal, albedo = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF} -- Debug values

-- Plane object ---
data PlaneRecord = Plane { pointPlane :: Vec3, normalPlane :: Vec3 }
  deriving (Show, Eq)

nullPlane :: PlaneRecord
nullPlane = Plane { pointPlane = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF, normalPlane = Vec3f 0xDEADBEEF 0xDEADBEEF 0xDEADBEEF }

-- Intersect functions --
-- Credit to closed-form for ray-sphere intersection math from https://rotgers.io/posts/ray-sphere-intersection/
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
  intersect rayDir rayOrigin plane = error "Plane intersection method is not implement"

  nullObject :: PlaneRecord
  nullObject = nullPlane


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

-- Lights --
data LightRecord = Light { posLight :: Vec3, normalLight :: Vec3, colorLight :: Vec3 }
  deriving (Show, Eq)

light1 :: LightRecord
light1 = Light { posLight = Vec3f (-1.0) 1.5 (0.25) , normalLight = Vec3f (-1.0) 0.0 0.0, colorLight = Vec3f 0.3 0.3 0.3} -- The normal is useless right now. Was planning rendering of light sources but never got to it.

-- Materials --
data Material = Metal | NonMetal
  deriving (Show, Eq)
