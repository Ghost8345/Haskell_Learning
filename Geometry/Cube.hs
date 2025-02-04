module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid -- we use qualified imports as we have the same name of functions across the Geometry modules

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side