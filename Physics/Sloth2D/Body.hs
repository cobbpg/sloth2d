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
    } deriving Show

fromShape :: Shape -> Body
fromShape shape = Body
    { shape = shape
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
    vs = vertices shape
    as = angles vs

withMass :: Body -> Float -> Body
body `withMass` mass = case abs mass of
    0 -> body { mass = 0, invMass = 0, angMass = 0, invAngMass = 0 }
    m -> body { mass = m, invMass = recip m, angMass = am, invAngMass = am' }
  where
    am = mass * momentOfInertia (shape body)
    am' = if am == 0 then 0 else recip am

withPosition :: Body -> (V2, Angle) -> Body
body `withPosition` (p,a) =
    body { position = p, angPosition = a
         , curVertices = vs, curAngles = as
         }
  where
    t = rotate a `withTranslation` p
    vs = V.map (t <>) (vertices (shape body))
    as = V.map (a +<) (edgeAngles body)

withVelocity :: Body -> (V2, Float) -> Body
body `withVelocity` (v,w) = body { velocity = v, angVelocity = w }

withShape :: Body -> Shape -> Body
body `withShape` shape =
    fromShape shape
    `withMass` mass body
    `withPosition` (position body, angPosition body)
    `withVelocity` (velocity body, angVelocity body)

transformation :: Body -> T2
transformation Body { position = p, angPosition = a } = rotate a `withTranslation` p
