module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Global as Global
import Math (pi, pow)
import Math as Math

data Point = Point
  { x ∷ Number
  , y ∷ Number
  }

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Point Picture

instance showPoint ∷ Show Point where
  show (Point { x, y }) = "Point { x: " <> show x <> ", y: " <> show y <> " }"

instance showShape ∷ Show Shape where
  show (Circle p r) =
    "Circle (" <> show p <> ") " <> show r
  show (Rectangle p w h) =
    "Rectangle (" <> show p <> ") " <> show w <> " " <> show h
  show (Line start end) =
    "Line (" <> show start <> ") (" <> show end <> ")"
  show (Text p text) =
    "Text (" <> show p <> ") " <> show text <> ""
  show (Clipped p pic) =
    "Clipped (" <> show p <> ") " <> (show $ showPicture pic)


origin ∷ Point
origin = Point { x: 0.0, y: 0.0 }


-- Exercise  5.14.1
c ∷ Shape
c = Circle origin radius
  where radius = 10.0


-- Exercise  5.14.2
center ∷ Point → Shape → Shape
center centerPoint@(Point centerPoint') = center'
  where
    center' (Circle _ r) = Circle centerPoint r
    center' (Rectangle _ w h) = Rectangle centerPoint w h
    center' (Line (Point start) (Point end))
      = Line (Point start') (Point end')
      where
        computeDelta p0 p1 p2 = p1 - (p1 - p2) / 2.0 - p0

        delta =
          { x: computeDelta centerPoint'.x start.x end.x
          , y: computeDelta centerPoint'.y start.y end.y
          }
        start' = { x: start.x - delta.x , y: start.y - delta.y }
        end' = { x: end.x - delta.x , y: end.y - delta.y }
    center' (Text _ s) = Text centerPoint s
    center' s = s

scale ∷ Number → Shape → Shape
scale factor = scale'
  where
    scale' (Circle p r) = Circle p (r * factor)
    scale' (Rectangle p w h) = Rectangle p (w * factor) (h * factor)
    scale' (Line (Point start) (Point end))
      = Line (Point start') (Point end')
      where
        delta = { x: (start.x - end.x) / 2.0 , y: (start.y - end.y) / 2.0 }
        start' =
          { x: start.x + delta.x * (factor - 1.0)
          , y: start.y + delta.y * (factor - 1.0)
          }
        end' =
          { x: end.x - delta.x * (factor - 1.0)
          , y: end.y - delta.y * (factor - 1.0)
          }
    scale' s = s

scaleByTwoAndCenterToOrigin ∷ Shape → Shape
scaleByTwoAndCenterToOrigin = scale 2.0 >>> center origin

-- Exercise 5.14.3
getString ∷ Shape → Maybe String
getString (Text p s) = Just s
getString _ = Nothing

--

type Picture = Array Shape

showPicture ∷ Picture → Array String
showPicture = map show

data Bounds = Bounds
  { top    ∷ Number
  , left   ∷ Number
  , bottom ∷ Number
  , right  ∷ Number
  }

instance showBounds ∷ Show Bounds where
  show (Bounds b) =
    "Bounds { top: " <> show b.top <>
    ", left: "      <> show b.left <>
    ", bottom: "    <> show b.bottom <>
    ", right: "     <> show b.right <>
    " }"

shapeBounds ∷ Shape → Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped p pic) = bounds pic

union ∷ Bounds → Bounds → Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect ∷ Bounds → Bounds → Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds ∷ Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds ∷ Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds ∷ Picture → Bounds
bounds = foldl combine emptyBounds
  where
    combine ∷ Bounds → Shape → Bounds
    combine b shape = union (shapeBounds shape) b


-- Exercise 5.17.1
area ∷ Shape → Number
area (Circle _ r) = (r `pow` 2.0) * pi
area (Rectangle _ w h) = w * h
area _ = 0.0
