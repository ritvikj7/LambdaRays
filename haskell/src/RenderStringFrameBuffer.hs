module RenderStringFrameBuffer where

-- TODO: This is still here in case we want an interactive window that is real-time before actually initiating the long offline render to picture
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
