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
  ]