module Physics.Sloth2D.Vector2D
    ( Angle
    , V2(..)
    , T2
    , unit, (*.), dot, cross, perpL, perpR
    , turn, turnL, turnNL, turnR, turnNR, parv
    , square, mag, norm, dir
    , inverse, (*^)
    , translate, rotate, scale
    , translationOf, rotationOf, scaleOf
    , withTranslation, withRotation, withScale
    ) where

import Data.Monoid

infixl 7 `dot`, `cross`
infixl 5 `turn`, `turnL`, `turnNL`, `turnR`, `turnNR`, `parv`

-- | An angle is a number between -pi and pi.
type Angle = Float

-- | 2D vector: a pair of coordinates.
data V2 = V {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    deriving (Show, Eq, Ord)

-- | 2D affine transformation. No shearing allowed, only translation,
-- rotation, and scaling. Transformations can be chained with
-- 'mappend', and 'mempty' is the identity transformation.
data T2 = T
    {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    deriving Show

instance Num V2 where
    V x1 y1 + V x2 y2 = V (x1+x2) (y1+y2)
    V x1 y1 - V x2 y2 = V (x1-x2) (y1-y2)
    V x1 y1 * V x2 y2 = V (x1*x2) (y1*y2)
    negate (V x y) = V (negate x) (negate y)
    abs (V x y) = V (abs x) (abs y)
    signum (V x y) = V (signum x) (signum y)
    fromInteger n = let n' = fromInteger n in V n' n'

-- | Unit vector with the given direction.
unit :: Angle -> V2
unit a = V (cos a) (sin a)

-- | Multiplication with a scalar.
(*.) :: V2 -> Float -> V2
V x y *. m = V (x*m) (y*m)

-- | Dot product.
dot :: V2 -> V2 -> Float
V x1 y1 `dot` V x2 y2 = x1*x2+y1*y2

-- | Perp-dot product (2D cross product).
cross :: V2 -> V2 -> Float
V x1 y1 `cross` V x2 y2 = x1*y2-y1*x2

-- | Vector rotated 90 degrees leftwards.
perpL :: V2 -> V2
perpL (V x y) = V (-y) x

-- | Vector rotated 90 degrees rightwards.
perpR :: V2 -> V2
perpR (V x y) = V y (-x)

-- | Relative direction of two vectors: @turn v1 v2@ equals @GT@ if
-- @v2@ takes a left turn with respect to @v1@, @LT@ if it is a right
-- turn, and @EQ@ if they are parallel.
turn :: V2 -> V2 -> Ordering
V x1 y1 `turn` V x2 y2 = compare (x1*y2) (y1*x2)

-- | @turnL v1 v2 == (turn v1 v2 == GT)@
turnL :: V2 -> V2 -> Bool
V x1 y1 `turnL` V x2 y2 = x1*y2 > y1*x2

-- | @turnNL v1 v2 == (turn v1 v2 /= GT)@
turnNL :: V2 -> V2 -> Bool
V x1 y1 `turnNL` V x2 y2 = x1*y2 <= y1*x2

-- | @turnR v1 v2 == (turn v1 v2 == LT)@
turnR :: V2 -> V2 -> Bool
V x1 y1 `turnR` V x2 y2 = x1*y2 < y1*x2

-- | @turnNR v1 v2 == (turn v1 v2 /= LT)@
turnNR :: V2 -> V2 -> Bool
V x1 y1 `turnNR` V x2 y2 = x1*y2 >= y1*x2

-- | @parv v1 v2 == (turn v1 v2 == EQ)@
parv :: V2 -> V2 -> Bool
V x1 y1 `parv` V x2 y2 = x1*y2 == y1*x2

-- | Vector length squared.
square :: V2 -> Float
square v = v `dot` v

-- | Vector length.
mag :: V2 -> Float
mag = sqrt . square

-- | The angle of a vector with respect to the X axis.
dir :: V2 -> Angle
dir (V x y) = atan2 y x

-- | Vector normalisation.
norm :: V2 -> V2
norm v@(V x y) = V (x*m) (y*m)
  where
    m = recip (mag v)

instance Monoid T2 where
    mempty = scale 1
    T rx1 ry1 tx1 ty1 `mappend` T rx2 ry2 tx2 ty2 = T rx ry tx ty
      where
        rx = rx1*rx2-ry1*ry2
        ry = ry1*rx2+rx1*ry2
        tx = rx1*tx2-ry1*ty2+tx1
        ty = ry1*tx2+rx1*ty2+ty1

-- | Inverse transformation
inverse :: T2 -> T2
inverse (T rx ry tx ty) = T (rx*m) (-ry*m) tx' ty'
  where
    m = recip (rx*rx+ry*ry)
    tx' = m*(-ry*ty-rx*tx)
    ty' = m*(ry*tx-rx*ty)

-- | Transformation applied to a vector.
(*^) :: T2 -> V2 -> V2
T rx ry tx ty *^ V x y = V x' y'
  where
    x' = rx*x-ry*y+tx
    y' = ry*x+rx*y+ty

-- | Transformation representing a translation.
translate :: V2 -> T2
translate (V x y) = T 1 0 x y

-- | Transformation representing a rotation.
rotate :: Angle -> T2
rotate a = T (cos a) (sin a) 0 0

-- | Transformation representing a scaling.
scale :: Float -> T2
scale m = T m 0 0 0

-- | The translation factor of a transformation.
translationOf :: T2 -> V2
translationOf (T _ _ tx ty) = V tx ty

-- | The rotation factor of a transformation.
rotationOf :: T2 -> Angle
rotationOf (T rx ry _ _) = dir (V rx ry)

-- | The scaling factor of a transformation.
scaleOf :: T2 -> Float
scaleOf (T rx ry _ _) = mag (V rx ry)

-- | Replacing the translation factor of a transformation.
withTranslation :: T2 -> V2 -> T2
T rx ry _ _ `withTranslation` V x y = T rx ry x y

-- | Replacing the rotation factor of a transformation.
withRotation :: T2 -> Angle -> T2
T rx ry tx ty `withRotation` a = T rx' ry' tx ty
  where
    V rx' ry' = unit a *. mag (V rx ry)

-- | Replacing the scaling factor of a transformation.
withScale :: T2 -> Float -> T2
T rx ry tx ty `withScale` m = T (m'*rx) (m'*ry) tx ty
  where
    m' = m / mag (V rx ry)
