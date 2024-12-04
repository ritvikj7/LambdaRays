module Camera where
import Vec3

-- Sampling information --
spp :: Int
spp = 7

-- Camera data --

-- TODO: Add FOV support
aspectRatio :: Float
aspectRatio = 16.0 / 9.0

imgWidth :: Int
imgWidth = 504

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
near = 1.0 -- TODO: Using focalLen instead; get rid of this - it's useless right now

focalLen :: Float
focalLen = 1.0 -- (sqrt ((fromIntegral imgWidth)^2 + (fromIntegral imgHeight)^2)) / ( 2*(tan (pi / 8)  ) ) <--- Method from StackOverflow to remove perspective distortion but not using it atm

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
createRay (x, y) = (imgOriginPixelCenter + (x *^ imgViewX) + (y *^ imgViewY)) - cameraPos -- Replace 'cameraPos' with 'o' when we have moving viewports

getImgCoords :: Int -> (Int, Int)
getImgCoords i = (i `mod` imgWidth, i `div` imgWidth)
