module PixelShader where

import Vec3
import Utils
import Camera
import ScenePrimitives

import System.Random
import System.IO.Unsafe (unsafePerformIO)

-- Just a CPU pixel shader --

-- Ambient light coefficient (controls the intensity of ambient light in the scene)
kA :: Float
kA = 0.005

-- Diffuse light coefficient (controls the intensity of diffuse reflection)
kD :: Float
kD = 0.8

-- Swapped out for per-sphere albedo
-- diffuseColor :: Vec3
-- diffuseColor = Vec3f 0.5 0.0 0.0

-- Specular light coefficient (controls the intensity of specular highlights)
kS :: Float
kS = 0.5

-- Swapped out for per-sphere albedo
-- ambientColor :: Vec3
-- ambientColor = Vec3f 0.025 0.0 0.15

{- Draw the background gradient for areas not intersected by objects. 
This creates a gradient from blue (sky) to white based on the y-coordinate of the ray -}
drawBackground :: (Float, Float) -> Vec3
drawBackground (_, y) = vecLerp (Vec3f 0.5 0.7 1.0) (Vec3f 1.0 1.0 1.0) (y / (fromIntegral imgHeight))


{- Shade a sphere by determining its material type and calculating the color contribution
Includes checks for shadows and recursive reflection for reflective materials -}
shadeSphere :: (Float, Float) -> Vec3 -> (Vec3, SphereRecord, Vec3) -> Int -> Vec3
shadeSphere (x,y) incidentRay (normal, sphere, hitPoint) depth = let shadowRay = vecNormalize ((posLight light1) - hitPoint)
                                                                     intersectRecord = (intersectAll shadowRay hitPoint (filter (/= sphere) listOfSpheres)) -- Got rid of shadow acne with this, let's goooo!
                                                                     sphereMaterial = (material sphere)
                                                                     sphereAlbedo = (albedo sphere)
                                                                     tShadow = (snd intersectRecord)
                                                                  in if (nearlyEqual tShadow (-1.0) 0.001) -- Self intersections will cause to set off sometimes (shadow acne)
                                                                     then if (sphereMaterial == NonMetal)
                                                                          then shadeNonMetalSphere (normal, sphere, hitPoint)
                                                                          else if (sphereMaterial == Metal)
                                                                          then shadeReflectiveSphere (x,y) incidentRay (normal, sphere, hitPoint) depth
                                                                          else error "Invalid material specified"
                                                                     else kA *^ sphereAlbedo

 -- Shade a reflective sphere by tracing the reflected ray --
shadeReflectiveSphere :: (Float, Float) -> Vec3 -> (Vec3, SphereRecord, Vec3) -> Int -> Vec3
shadeReflectiveSphere (x,y) incidentRay (normal, sphere, hitPoint) 0 = (albedo sphere)
shadeReflectiveSphere (x,y) incidentRay (normal, sphere, hitPoint) depth = let reflectRay = vecNormalize (vecReflect (vecNormalize incidentRay) normal) -- TODO: Gotta clean up these normalizations. I'm throwing them out like free candy lol
                                                                               (Vec3f albedoR albedoG albedoB) = (albedo sphere)
                                                                               intersectRecord = (intersectAll reflectRay hitPoint (filter (/= sphere) listOfSpheres)) -- Filter is to prevent shadow acne (self-intersection)
                                                                               newSphere = (fst intersectRecord)
                                                                               tReflect = (snd intersectRecord)
                                                                            in if tReflect >= 0.0
                                                                               then let newNormal = computeNormal intersectRecord reflectRay hitPoint
                                                                                        newHitPoint = getRayAtT hitPoint tReflect reflectRay
                                                                                        (Vec3f cx cy cz) = shadeSphere (x, y) incidentRay (newNormal, newSphere, newHitPoint) (depth - 1)
                                                                                    in (Vec3f (albedoR * cx) (albedoG * cy) (albedoB * cz))
                                                                               else let (Vec3f bx by bz) = drawBackground (x,y)
                                                                                    in (Vec3f (albedoR * bx) (albedoG * by) (albedoB * bz)) -- Should add Hadamard product for vectors because this is tediouuuus

-- Shade Non-Metallic surface using Blinn-Phong BRDF (yeah, I know it's not the 90s anymore but it's simple)
-- Credit to Blinn-Phong BRDF from lectures in CPSC 314
shadeNonMetalSphere :: (Vec3, SphereRecord, Vec3) -> Vec3
shadeNonMetalSphere (normal, sphere, hitPoint) = let shadowRay = vecNormalize ((posLight light1) - hitPoint)
                                                     view = vecNormalize (cameraPos - hitPoint)
                                                     half = vecNormalize (shadowRay + view)
                                                     lightColor = (colorLight light1)
                                                     sphereAlbedo = (albedo sphere)

                                                     ambient = kA *^ sphereAlbedo
                                                     diffuse = (kD * (max (dot normal shadowRay) 0.0)) *^ sphereAlbedo
                                                     specular = (kS * ((max (dot normal half) 0.0) ** 32.0)) *^ lightColor

                                                     (Vec3f cx cy cz) = ambient + diffuse + specular
                                                 in (Vec3f (min cx 1.0) (min cy 1.0) (min cz 1.0)) -- Change from clamp tonemapper to filmic tonemapper if get HDR elements

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
                                              rayDepth = 3
                                          in shadeSphere (offsetX, offsetY) ray ((Vec3f nx ny nz), sphere, hitPoint) rayDepth + (samplePixel (x, y) (numSamples - 1))
                                     else drawBackground (offsetX, offsetY) + (samplePixel (x, y) (numSamples - 1))

-- Utils --
-- Compute normalized normal from sphere intersection point to sphere center
computeNormal :: (SphereRecord, Float) -> Vec3 -> Vec3 -> Vec3
computeNormal (sphere, t) ray o = if t >= 0.0
                                  then  (1.0 / (radius sphere)) *^ (((t *^ ray) + o) - (position sphere))
                                  else (Vec3f 0.0 0.0 0.0)

generatePixelOffset :: (Float, Float) -> (Float, Float)
generatePixelOffset (x, y) = let offsetX = unsafePerformIO $ generateRandomNum (-0.5) 0.5
                                 offsetY = unsafePerformIO $ generateRandomNum (-0.5) 0.5
                             in (x + offsetX, y + offsetY)
