{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Vec3 where

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

    (*) = error "Vector multiplication is errorneous: use dot or cross product instead" -- TODO: Enable hadamard product here?

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
