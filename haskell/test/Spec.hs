import Lib
import Test.Tasty
import Test.Tasty.HUnit

v1 :: Vec3
v1 = Vec3f 1 2 3

v2 :: Vec3
v2 = Vec3f 4 5 6

-- Additional test vectors and spheres for testing intersections
sphere1 :: SphereRecord
sphere1 = Sphere { position = Vec3f 0.0 0.0 0.0, radius = 1.0 }

sphere2 :: SphereRecord
sphere2 = Sphere { position = Vec3f 0.0 0.0 5.0, radius = 1.0 }

-- New vectors for edge case tests
zeroVector :: Vec3
zeroVector = Vec3f 0.0 0.0 0.0

largeVector :: Vec3
largeVector = Vec3f 1e6 1e6 1e6

main :: IO ()
main = defaultMain $ testGroup "Vec3 Tests" 
  [ testCase "Addition works" $ do
      Vec3f 5 7 9 @?= (v1 + v2)
  , testCase "Subtraction works" $ do
      Vec3f (-3) (-3) (-3) @?= (v1 - v2)
  , testCase "Negation works" $ do
      Vec3f (-1) (-2) (-3) @?= negate v1
  , testCase "Scalar multiplication (left) works" $ do
      Vec3f 2.0 4.0 6.0 @?= (2.0 *^ v1)
  , testCase "Scalar multiplication (right) works" $ do
      Vec3f 3.0 6.0 9.0 @?= (v1 *. 3.0)
  , testCase "Dot product works" $ do
      32.0 @?= (v1 `dot` v2)
  , testCase "Cross product works" $ do
      Vec3f (-3.0) 6.0 (-3.0) @?= (v1 `cross` v2)
  , testCase "Vector length works" $ do
      sqrt 14.0 @?= vecLength v1
  , testCase "Vector distance works" $ do
      sqrt 32.0 @?= vecDistance v1 v2
  , testCase "Vector normalization works" $ do
      Vec3f (1.0 / sqrt 14.0) (2.0 / sqrt 14.0) (3.0 / sqrt 14.0) @?= vecNormalize v1
  , testCase "Ray-Sphere intersection test (hit)" $ do
      let rayDirection = Vec3f 0.0 0.0 (-1.0)
      let result = intersect rayDirection sphere1 (Vec3f 0.0 0.0 5.0)
      result @?= True
  , testCase "Ray-Sphere intersection test (miss)" $ do
      let rayDirection = Vec3f 0.0 0.0 (-1.0)
      let result = intersect rayDirection sphere2 (Vec3f 0.0 0.0 0.0)
      result @?= False
  , testCase "Zero vector addition" $ do
      v1 @?= (v1 + (0 *^ v1))
  , testCase "Zero vector normalization" $ do
      Vec3f 0.0 0.0 0.0 @?= vecNormalize zeroVector
  , testCase "Large vector normalization" $ do
      let normalized = vecNormalize largeVector
      assertBool "Normalized vector dot product is approximately 1" $ abs (1.0 - (normalized `dot` normalized)) < 1e-6
  , testCase "Boundary condition - large values" $ do
      let largeVec = Vec3f 1e6 1e6 1e6
      largeVec @?= (largeVec - zeroVector)
  ]