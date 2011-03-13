module Physics.Sloth2D.Body where

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

import Physics.Sloth2D.Geometry2D
import Physics.Sloth2D.Vector2D
import Physics.Sloth2D.Shape

data Body = Body
    { shape :: Shape
    , edgeAngles :: Vector Float
    , mass :: Float
    , invMass :: Float
    , angMass :: Float
    , invAngMass :: Float
    , position :: V2
    , velocity :: V2
    , angPosition :: Float
    , angVelocity :: Float
    , curVertices :: Vector V2
    , curAngles :: Vector Float
    }

fromShape :: Shape -> Body
fromShape s = Body
    { shape = s
    , edgeAngles = as
    , mass = 0
    , invMass = 0
    , angMass = 0
    , invAngMass = 0
    , position = V 0 0
    , velocity = V 0 0
    , angPosition = 0
    , angVelocity = 0
    , curVertices = vs
    , curAngles = as
    }
  where
    vs = vertices s
    as = angles vs

withMass :: Body -> Float -> Body
body `withMass` mass = case abs mass of
    0 -> body { mass = 0, invMass = 0, angMass = 0, invAngMass = 0 }
    m -> body { mass = m, invMass = recip m, angMass = am, invAngMass = am' }
  where
    am = mass * momentOfInertia (shape body)
    am' = if am == 0 then 0 else recip am

transformation :: Body -> T2
transformation Body { position = p, angPosition = a } = translate p `mappend` rotate a

{-
withShape :: Body -> Vector V2 -> Body
body `withShape` s = body { shapeOf = s, shapeEdges = es, shapeAngles = as, momentRatio = r }
                     `withTrans` transOf body `withMass` massOf body
  where
    es = V.map norm (edges s)
    as = angles s
    r = moment s * (scaleOf (transOf body))^2

withTrans :: Body -> T2 -> Body
body `withTrans` t = body { transOf = t, tshapeOf = ts, tshapeEdges = tse, tshapeAngles = tsa }
  where
    r = rotationOf t
    ts = V.map (t <>) (shapeOf body)
    -- Note: below we assume that the transformation preserves angles!
    -- This is ensured by the interface exposed in the Vector2D
    -- module.
    tse = V.map (rotate r <>) (shapeEdges body)
    tsa = V.map (r +<) (shapeAngles body)
-}
