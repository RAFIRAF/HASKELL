module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Num a => a -> a
volume a = Cuboid.volume a a a

area :: Num a => a -> a
area a = Cuboid.area a a a