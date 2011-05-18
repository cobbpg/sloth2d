module Physics.Sloth2D.Dynamics where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Physics.Sloth2D.Body
import Physics.Sloth2D.Stepper
import Physics.Sloth2D.Vector2D

type CollisionRelation = CollisionLayer -> CollisionLayer -> Bool

data Dynamics = Dynamics
    { manager :: Stepper
    , bodies :: IntMap Body
    , gravity :: V2
    , collisionRelation :: CollisionRelation
    , nextId :: Int
    }

dynamicWorld :: Float -> Float -> V2 -> Dynamics
dynamicWorld tstep dtmax grav = Dynamics
    { manager = stepper tstep dtmax
    , bodies = M.empty
    , gravity = grav
    , collisionRelation = \_ _ -> True
    , nextId = 0
    }

addBodies :: Dynamics -> [Body] -> (Dynamics, [Int])
addBodies world newBodies =
    (world { bodies = bodies', nextId = nextId' }, [firstId..nextId'-1])
  where
    firstId = nextId world
    (nextId',bodies') = foldl' add (firstId, bodies world) newBodies
    add (i,m) e = m' `seq` (i',m')
      where
        i' = i+1
        m' = M.insert i e m

withCollisionRelation :: Dynamics -> CollisionRelation -> Dynamics
world `withCollisionRelation` rel = world { collisionRelation = rel }

collisionPairs :: [(CollisionLayer, CollisionLayer)] -> CollisionRelation
collisionPairs [] = \_ _ -> False
collisionPairs pairs = rel
  where
    rel l1 l2 = l1 >= 0 && l1 <= vmax && l2 >= 0 && l2 <= vmax && v ! (l1*(vmax+1)+l2)
    vmax = maximum (concatMap (\(x,y) -> [x,y]) pairs)
    v = V.fromList [(x,y) `elem` pairs || (y,x) `elem` pairs | x <- [0..vmax], y <- [0..vmax]]

advancedBy :: Dynamics -> Float -> Dynamics
world `advancedBy` dt = world
    { manager = m'
    , bodies = foldl' newBodies (bodies world) (replicate n sdt)
    }
  where
    iterations = 10
    (n,m') = advance dt (manager world)
    sdt = timeStep m'
    newBodies bodies dt = M.fromList $
                          zip (M.keys bodies) (V.toList (V.map (integrate dt) (resolveCollisions iterations bs)))
      where
        bs = addGravity (gravity world *. dt) (V.map shiftBody (V.fromList (M.elems bodies)))
        addGravity g bs = if g /= V 0 0 then V.map addG bs else bs
          where
            addG b = if mass b == 0 then b else b `nudgedBy` (g,0)

        colrel = collisionRelation world
        resolveCollisions n bs
            | n < 1     = bs
            | otherwise = resolveCollisions (n-1) (resolve (n/iterations) bs)
        resolve eps bs = V.accum addImpact bs [r | i1 <- [0..num], i2 <- [i1+1..num], r <- check i1 i2]
          where
            num = V.length bs - 1
            addImpact body (p,v,w) = body `nudgedBy` (v,w) `movedBy` (p*.0.6,0)
            check i1 i2 = if colrel (layer b1) (layer b2) then
                              case collisionResponse eps b1 b2 of
                                  Nothing -> []
                                  Just (p1,v1,w1,p2,v2,w2) -> [(i1,(p1,v1,w1)),(i2,(p2,v2,w2))]
                          else []
              where
                b1 = bs ! i1
                b2 = bs ! i2
