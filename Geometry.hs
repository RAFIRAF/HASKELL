module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , cuboidArea
  , cuboidVolume
  ) where

sphereVolume :: Floating a => a -> a
sphereVolume r = 4/3 * pi * r^3

sphereArea :: Floating a => a -> a
sphereArea r = 4 * pi * r^2

cubeVolume :: Num a => a -> a
cubeVolume a = cuboidVolume a a a

cubeArea :: Num a => a -> a
cubeArea a = cuboidArea a a a

cuboidVolume :: Num a => a -> a -> a -> a
cuboidVolume a b c = rectangleArea a b *c

cuboidArea :: Num a => a -> a -> a -> a
cuboidArea a b c = 2 * rectangleArea a b + 2 * rectangleArea b c + 2 * rectangleArea a c

rectangleArea :: Num a => a -> a -> a
rectangleArea a b = a * b