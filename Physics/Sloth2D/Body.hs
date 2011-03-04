module Physics.Sloth2D.Body where

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

import Physics.Sloth2D.Geometry2D
import Physics.Sloth2D.Vector2D

data Body = Body
    { mass :: Float
    , mass' :: Float
    , angMassRatio :: Float
    , angMass :: Float
    , angMass' :: Float
    , shape :: Vector V2
    , shapeEdges :: Vector V2
    , shapeAngles :: Vector Float
    , trans :: T2
    , tshape :: Vector V2
    , tshapeEdges :: Vector V2
    , tshapeAngles :: Vector Float
    }

defaultBody :: Body
defaultBody = Body
    { mass = 0
    , mass' = 0
    , angMassRatio = 1
    , angMass = 0
    , angMass' = 0
    , shape = V.empty
    , shapeEdges = V.empty
    , shapeAngles = V.empty
    , trans = mempty
    , tshape = V.empty
    , tshapeEdges = V.empty
    , tshapeAngles = V.empty
    }

convexHullShape :: Vector V2 -> Body
convexHullShape vs = defaultBody { angMassRatio = r } `withShape` vs'
  where
    vs' = convexHull vs
    r = moment vs'

withShape :: Body -> Vector V2 -> Body
body `withShape` s = body { shape = s, shapeEdges = es, shapeAngles = as }
                     `withTrans` trans body
  where
    es = V.map norm (edges s)
    as = angles s

withMass :: Body -> Float -> Body
body `withMass` m = case abs m of
    0 -> body { mass = 0, mass' = 0, angMass = 0, angMass' = 0 }
    m -> body { mass = m, mass' = recip m, angMass = r*m, angMass' = recip (r*m) }
  where
    r = angMassRatio body

withTrans :: Body -> T2 -> Body
body `withTrans` t = body { trans = t, tshape = ts, tshapeEdges = tse, tshapeAngles = tsa }
  where
    r = rotationOf t
    ts = V.map (t <>) (shape body)
    -- Note: below we assume that the transformation preserves angles!
    -- This is ensured by the interface exposed in the Vector2D
    -- module.
    tse = V.map (rotate r <>) (shapeEdges body)
    tsa = V.map (r +<) (shapeAngles body)
