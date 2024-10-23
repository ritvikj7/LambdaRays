import Lib
import Test.Tasty
import Test.Tasty.HUnit

v1 :: Vec3
v1 = Vec3f 1 2 3

v2 :: Vec3
v2 = Vec3f 4 5 6

main :: IO ()
main = defaultMain $ testGroup "Vec3 Tests" 
  [ testCase "Addition works" $ do
      (v1 + v2) @?= Vec3f 5 7 9
  , testCase "Subtraction works" $ do
      (v1 - v2) @?= Vec3f (-3) (-3) (-3)
  , testCase "Negation works" $ do
      negate v1 @?= Vec3f (-1) (-2) (-3)
  , testCase "Scalar multiplication (left) works" $ do
      (2.0 *^ v1) @?= Vec3f 2.0 4.0 6.0
  , testCase "Scalar multiplication (right) works" $ do
      (v1 *. 3.0) @?= Vec3f 3.0 6.0 9.0
  , testCase "Dot product works" $ do
      (v1 `dot` v2) @?= 32.0
  , testCase "Cross product works" $ do
    (v1 `cross` v2) @?= Vec3f (-3.0) 6.0 (-3.0)
  , testCase "Vector length works" $ do
      vecLength v1 @?= sqrt 14.0
  , testCase "Vector distance works" $ do
      vecDistance v1 v2 @?= sqrt 32.0
  , testCase "Vector normalization works" $ do
      vecNormalize v1 @?= Vec3f (1.0 / sqrt 14.0) (2.0 / sqrt 14.0) (3.0 / sqrt 14.0)
  ]