module Physics.Sloth2D.Body where

import Data.List
import Data.Maybe
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
    , masses :: (Float, Float, Float, Float)  -- mass, 1/mass, moment, 1/moment
    , curState :: DynamicState
    , curGeometry :: Vector V2
    , prevState :: DynamicState
    , prevGeometry :: Vector V2
    } deriving Show

shiftBody :: Body -> Body
shiftBody body = body { prevState = curState body, prevGeometry = curGeometry body }

integrate :: Float -> Body -> Body
integrate dt body = body `movedBy` (curV body*.dt, curW body*dt)

fromShape :: Shape -> Body
fromShape shape = Body
    { shape = shape
    , masses = (0,0,0,0)
    , curState = st
    , curGeometry = vs
    , prevState = st
    , prevGeometry = vs
    }
  where
    vs = vertices shape
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
    body { curState = st, curGeometry = vs }
  where
    t = transRot p a
    vs = V.map (t <>) (vertices (shape body))

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
    `withState` curState body

movedBy :: Body -> (V2, Angle) -> Body
body@Body { curState = Dyn p v a w } `movedBy` (p',a') =
    body `withState` Dyn (p+p') v (a+<a') w

nudgedBy :: Body -> (V2, Float) -> Body
body@Body { curState = Dyn p v a w } `nudgedBy` (v',w') =
    body { curState = Dyn p (v+v') a (w+w') }

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

collisionResponse :: Float -> Body -> Body -> Maybe (V2, V2, Float, V2, V2, Float)
collisionResponse eps b1 b2 = if null imps then Nothing
                              else Just (mulMasses (foldl' addImp nullImp imps))
  where
    Body { masses = (_,m1',_,i1'), curState = Dyn p1 v1 a1 w1, curGeometry = vs1 } = b1
    Body { masses = (_,m2',_,i2'), curState = Dyn p2 v2 a2 w2, curGeometry = vs2 } = b2
    m12' = m1'+m2'
    m12'' = recip m12'
    tooFar = square (p1-p2) > (maxRadius (shape b1)+maxRadius (shape b2))^2
    (d2,ds,fs) = convexSeparations vs1 vs2
    imps = if tooFar || ds > 0 || (m1' == 0 && m2' == 0) then []
           else [mkImp r1 r2 | (_,(r1,r2)) <- fs]
    mkImp r1 r2
        | d < 0     = Left n
        | otherwise = Right (n,j,ta,tb)
      where
        n = r1-r2
        ra = r1-p1
        rb = r2-p2
        vab = v1+perpL ra*.w1-v2-perpL rb*.w2
        d = vab `dot` n
        ta = ra `cross` n
        tb = rb `cross` n
        j = (1+eps)*d/(m12'*square n+i1'*ta*ta+i2'*tb*tb)
    nullImp = (V 0 0, V 0 0, 0, 0, 0)
    addImp (n,dp,j,ta,tb) (Left n') = (n+n',dp+n',j,ta,tb)
    addImp (n,dp,j,ta,tb) (Right (n',j',ta',tb')) = (n+n',dp,j+j',ta+ta',tb+tb')
    fsl = recip (fromIntegral (length imps))
    fsl' = -fsl
    mulMasses (n,dp,j,ta,tb) = (dp*.(m1'*m12''*fsl'),n*.(j*m1'*fsl'),ta*j*i1'*fsl'
                               ,dp*.(m2'*m12''*fsl ),n*.(j*m2'*fsl ),tb*j*i2'*fsl)

transRot :: V2 -> Angle -> T2
transRot v a = rotate a `withTranslation` v
