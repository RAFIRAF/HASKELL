module Shapes
( Point
, Shape
, surface
, nudge
, baseCircle
, baseRect
) where
data Shape = Circle Point Float | Rectangle Point Point deriving Show
data Point = Point Float Float deriving Show
surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2-x1)*(abs $ y2-y1)

nudge :: Float -> Float -> Shape -> Shape
nudge x y (Circle (Point x0 y0) r) = (Circle (Point (x0+x) (y0+y)) r)
nudge x y (Rectangle (Point x1 y1) (Point x2 y2)) = (Rectangle (Point (x1+x) $y1+y) $Point (x2+x) $y2+y) 

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) $ Point (w) h