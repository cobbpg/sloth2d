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

withState :: Body -> DynamicState -> Body
body `withState` st@(Dyn p _ a _) =
    body { curState = st, curGeometry = (vs,as) }
  where
    t = transRot p a
    vs = V.map (t <>) (vertices (shape body))
    as = V.map (a +<) (edgeAngles body)

withPosition :: Body -> (V2, Angle) -> Body
body@Body { curState = Dyn _ v _ w } `withPosition` (p,a) =
    body `withState` Dyn p v a w

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
curTransformation Body { curState = Dyn p _ a _ } = transRot p a

curP :: Body -> V2
curP Body { curState = Dyn p _ _ _ } = p

curV :: Body -> V2
curV Body { curState = Dyn _ v _ _ } = v

curA :: Body -> Angle
curA Body { curState = Dyn _ _ a _ } = a

curW :: Body -> Float
curW Body { curState = Dyn _ _ _ w } = w

curT :: Body -> T2
curT Body { curState = Dyn p _ a _ } = transRot p a

prevP :: Body -> V2
prevP Body { prevState = Dyn p _ _ _ } = p

prevV :: Body -> V2
prevV Body { prevState = Dyn _ v _ _ } = v

prevA :: Body -> Angle
prevA Body { prevState = Dyn _ _ a _ } = a

prevW :: Body -> Float
prevW Body { prevState = Dyn _ _ _ w } = w

prevT :: Body -> T2
prevT Body { prevState = Dyn p _ a _ } = transRot p a

position :: Float -> Body -> V2
position t Body { curState = Dyn p _ _ _, prevState = Dyn p' _ _ _ } = p*.t+p'*.(1-t)

orientation :: Float -> Body -> Angle
orientation t Body { curState = Dyn _ _ a _, prevState = Dyn _ _ a' _ } = alerp a' a t

velocity :: Float -> Body -> V2
velocity t Body { curState = Dyn _ v _ _, prevState = Dyn _ v' _ _ } = v*.t+v'*.(1-t)

angularVelocity :: Float -> Body -> Float
angularVelocity t Body { curState = Dyn _ _ _ w, prevState = Dyn _ _ _ w' } = w*t+w'*(1-t)

transformation :: Float -> Body -> T2
transformation t body = transRot (position t body) (orientation t body)

collisionResponse :: Float -> Body -> Body -> [(V2, Float, V2, Float)]
collisionResponse eps b1 b2 = imps
  where
    Body { masses = (_,m1',_,i1'), curState = Dyn p1 v1 a1 w1, curGeometry = (vs1,as1) } = b1
    Body { masses = (_,m2',_,i2'), curState = Dyn p2 v2 a2 w2, curGeometry = (vs2,as2) } = b2
    t1 = curT b1
    t2 = curT b2
    tooFar = square (p1-p2) > (maxRadius (shape b1)+maxRadius (shape b2))^2
    (d2,ds,fs) = convexSeparations vs1 as1 vs2 as2
    fsl = recip (fromIntegral (length fs))
    imps = if tooFar || ds > 0 then [] else [mkImp b r1 r2 | (b,_,_,r1,r2) <- fs]
    mkImp False r1 r2 = mkImp True r2 r1
    mkImp True r1 r2 = (j*.(-m1'), -i1'*(ra `cross` j), j*.m2', i2'*(rb `cross` j))
      where
        ra = r1-p1
        rb = r2-p2
        j = (v1+perpL ra*.w1-v2-perpL rb*.w2)*.
            (fsl*(1+eps)/(m1'+m2'+i1'*square ra+i2'*square rb))

transRot :: V2 -> Angle -> T2
transRot v a = rotate a `withTranslation` v

{-

1. Collision detection and modeling. - eps = 1
2. Advance the velocities using equation 2.
3. Contact resolution. - eps = 0
4. Advance the positions using equation 1.

-}
