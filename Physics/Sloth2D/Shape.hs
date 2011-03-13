module Physics.Sloth2D.Shape
    ( Shape
    , polygonShape, convexHullShape
    , vertices, surfaceArea, maxRadius
    , momentOfInertia, momentOfInertiaAt
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Physics.Sloth2D.Geometry2D
import Physics.Sloth2D.Vector2D

-- | A shape is a planar object with the centroid at the origin and
-- counter-clockwise orientation.
data Shape = Shape
    { vertices :: Vector V2    -- ^ The vertices of a shape.
    , momentOfInertia :: Float -- ^ The moment of inertia of a shape (memoised).
    , surfaceArea :: Float     -- ^ The surface area of a shape (memoised).
    , maxRadius :: Float       -- ^ The largest distance from the origin (memoised).
    } deriving Show

-- | A shape derived from a simple polygon.  The orientation of the
-- vertices can be both clockwise and counter-clockwise.
polygonShape :: Vector V2 -> Shape
polygonShape vs = Shape vs' (moment vs') a r
  where
    c = centroid vs
    vs' = (if area vs > 0 then id else V.reverse) (V.map (subtract c) vs)
    a = abs (area vs)
    r = if V.null vs then 0 else V.maximum (V.map square vs')

-- | A shape derived from the convex hull of a collection of vertices.
convexHullShape :: Vector V2 -> Shape
convexHullShape = polygonShape . convexHull

-- | The moment of inertia of a shape with respect to a given position
-- in its own coordinate system.
momentOfInertiaAt :: V2 -> Shape -> Float
momentOfInertiaAt v = (+ square v) . momentOfInertia
