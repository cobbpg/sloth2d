module Physics.Sloth2D.Body where

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

import Physics.Sloth2D.Geometry2D
import Physics.Sloth2D.Vector2D
import Physics.Sloth2D.Shape

data DynamicState = Dyn
    {-# UNPACK #-} !V2 {-# UNPACK #-} !V2        -- position, velocity
    {-# UNPACK #-} !Angle {-# UNPACK #-} !Float  -- orientation, angular velocity
    deriving Show

data Body = Body
    { shape :: Shape
    , edgeAngles :: Vector Float
    , masses :: (Float, Float, Float, Float)  -- mass, 1/mass, moment, 1/moment
    , curState :: DynamicState
    , curGeometry :: (Vector V2, Vector Float)  -- vertices, edge directions
    , prevState :: DynamicState
    , prevGeometry :: (Vector V2, Vector Float)  -- vertices, edge directions
    } deriving Show

shiftBody :: Body -> Body
shiftBody body = body { prevState = curState body, prevGeometry = curGeometry body }

fromShape :: Shape -> Body
fromShape shape = Body
    { shape = shape
    , edgeAngles = as
    , masses = (0,0,0,0)
    , curState = st
    , curGeometry = (vs,as)
    , prevState = st
    , prevGeometry = (vs,as)
    }
  where
    vs = vertices shape
    as = angles vs
    st = Dyn (V 0 0) (V 0 0) 0 0

withMass :: Body -> Float -> Body
body `withMass` mass = body { masses = (m,m',am,am') }
  where
    m = abs mass
    (m',am) = if m == 0 then (0,0)
              else (recip m, m * momentOfInertia (shape body))
    am' = if am == 0 then 0 else recip am

withPosition :: Body -> (V2, Angle) -> Body
body@Body { curState = Dyn _ v _ w } `withPosition` (p,a) =
    body { curState = Dyn p v a w, curGeometry = (vs,as) }
  where
    t = rotate a `withTranslation` p
    vs = V.map (t <>) (vertices (shape body))
    as = V.map (a +<) (edgeAngles body)

withVelocity :: Body -> (V2, Float) -> Body
body@Body { curState = Dyn p _ a _ } `withVelocity` (v,w) =
    body { curState = Dyn p v a w }

withShape :: Body -> Shape -> Body
body `withShape` shape =
    fromShape shape
    `withMass` mass body
    `withPosition` (curP body, curA body)
    `withVelocity` (curV body, curW body)

mass :: Body -> Float
mass Body { masses = (m,_,_,_) } = m

invMass :: Body -> Float
invMass Body { masses = (_,m,_,_) } = m

angMass :: Body -> Float
angMass Body { masses = (_,_,m,_) } = m

invAngMass :: Body -> Float
invAngMass Body { masses = (_,_,_,m) } = m

curTransformation :: Body -> T2
curTransformation Body { curState = Dyn p _ a _ } = rotate a `withTranslation` p

curP :: Body -> V2
curP Body { curState = Dyn p _ _ _ } = p

curV :: Body -> V2
curV Body { curState = Dyn _ v _ _ } = v

curA :: Body -> Angle
curA Body { curState = Dyn _ _ a _ } = a

curW :: Body -> Float
curW Body { curState = Dyn _ _ _ w } = w

transformation :: Float -> Body -> T2
transformation t body = rotate (orientation t body) `withTranslation` position t body

position :: Float -> Body -> V2
position t Body { curState = Dyn p _ _ _, prevState = Dyn p' _ _ _ } = p*.t+p'*.(1-t)

orientation :: Float -> Body -> Angle
orientation t Body { curState = Dyn _ _ a _, prevState = Dyn _ _ a' _ } = alerp a' a t

velocity :: Float -> Body -> V2
velocity t Body { curState = Dyn _ v _ _, prevState = Dyn _ v' _ _ } = v*.t+v'*.(1-t)

angularVelocity :: Float -> Body -> Float
angularVelocity t Body { curState = Dyn _ _ _ w, prevState = Dyn _ _ _ w' } = w*t+w'*(1-t)
