{-# LANGUAGE BangPatterns #-}

module Physics.Sloth2D.Geometry2D where

import Data.List
import Data.Ord
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V

import Physics.Sloth2D.Vector2D

data VertexType = TopCap | BottomCap | TopCup | BottomCup | Side
    deriving Show

data Vertex = Vtx
    { idx :: Int
    , prev :: Int
    , next :: Int
    , vtype :: VertexType
    , px :: Float
    , py :: Float
    } deriving Show

type MonotoneSegment = ([Int],[Int])

-- | Checking whether an angle is within a given interval.
between :: Angle -> (Angle,Angle) -> Bool
a `between` (a1,a2)
    | a1 <= a2  = a >= a1 && a <= a2
    | otherwise = a >= a1 || a <= a2

infixl 6 +<

-- | The sum of two angles.
(+<) :: Angle -> Angle -> Angle
a1 +< a2 = if a < -pi then a+2*pi
           else if a > pi then a-2*pi
                else a
  where
    a = a1+a2

-- | Linear interpolation between two angles along the smaller arc.
alerp :: Angle -> Angle -> Float -> Angle
alerp a1 a2 t = a1+<(a2+<(-a1))*t

-- | Applying a binary function to consecutive pairs in a vector with
-- wrap-around.
pairsWith :: (a -> a -> b) -> Vector a -> Vector b
pairsWith f vs
    | V.null vs = V.empty
    | otherwise = V.zipWith f vs (V.snoc (V.tail vs) (V.head vs))

-- | The edge vectors of a polygon given as a list of vertices.
edges :: Vector V2 -> Vector V2
edges vs = if V.length vs < 2 then V.empty else pairsWith (flip (-)) vs

-- | The absolute angles (with respect to the x axis) of the edges of
-- a polygon given as a list of vertices.
angles :: Vector V2 -> Vector Angle
angles = V.map dir . edges

-- | The signed area of a simple polygon (positive if vertices are in
-- counter-clockwise order).
area :: Vector V2 -> Float
area vs = 0.5 * V.sum (pairsWith cross vs)

-- | The centroid of a simple polygon.
centroid :: Vector V2 -> V2
centroid vs
    | V.null vs = V 0 0
    | otherwise = divsum (V.foldl1' accum (pairsWith gen vs))
  where
    gen v1 v2 = let c = v1 `cross` v2 in (c,(v1+v2)*.c)
    accum (!c1,!v1) (c2,v2) = (c1+c2,v1+v2)
    divsum (c,v)
        | c /= 0    = v*.(recip (3*c))
        | otherwise = (V.minimum vs+V.maximum vs)*.0.5

-- | The moment of inertia of a simple polygon with respect to the origin.
moment :: Vector V2 -> Float
moment vs
    | V.length vs < 3 = 0
    | otherwise       = divsum (V.foldl1' accum (pairsWith gen vs))
  where
    gen v1 v2 = let c = v2 `cross` v1 in (c,(v1 `dot` (v1+v2) + square v2)*c)
    accum (!s1,!s2) (p1,p2) = (s1+p1,s2+p2)
    divsum (s1,s2)
        | s1 /= 0   = s2/(6*s1)
        | otherwise = 0

-- | The convex hull of a collection of vertices in counter-clockwise
-- order. (Andrew's Monotone Chain Algorithm)
convexHull :: Vector V2 -> Vector V2
convexHull vs = case compare (V.length vs) 2 of
    LT -> vs
    EQ -> V.fromList . nub . V.toList $ vs
    GT -> V.fromList (avs' ++ bvs')
  where
    svs = V.modify V.sort vs
    vmin = V.head svs
    vmax = V.last svs
    vd = vmax-vmin

    (avs,bvs) = V.partition (\v -> vd `turnNR` v-vmax) . V.init . V.tail $ svs
    avs' = if V.null avs then [vmin]
           else tail . V.foldl' (flip addVertex) [V.head avs,vmin] $ V.snoc (V.tail avs) vmax
    bvs' = if V.null bvs then [vmax]
           else tail . V.foldr' addVertex [V.last bvs,vmax] $ V.cons vmin (V.init bvs)

    addVertex v (v1:vs@(v2:_)) | v1-v2 `turnNR` v-v1 = addVertex v vs
    addVertex v vs = v:vs

-- | Monotone decomposition of a simple polygon.
monotoneDecomposition :: Vector V2 -> [MonotoneSegment]
monotoneDecomposition vs = (map getIndices . snd) (V.foldl' addVertex ([], []) scvs)
  where
    cw = area vs < 0
    ovs = if cw then vs else V.reverse vs
    getIndices (l,r) = if cw then (map idx l, map idx r)
                       else (map idx' l, map idx' r)
      where
        idx' v = V.length vs - 1 - idx v
    addVertex (mss, out) v = case vtype v of
      -- open new monotone segment with this sole vertex
      TopCap -> (([v], [v]) : mss, out)
      -- split monotone segment: all vertices are added to left side,
      -- only last two to right; this is the only case where we need
      -- to check geometry to find the matching segment
      BottomCap -> let (mss',(msl,msr):mss'') = break isContained mss
                       ms' = (msl, v : msr)
                       ms'' = ([v, head msr], [head msr])
                   in (mss' ++ ms':ms'':mss'', out)
      -- close the segment on the right side using the join vertex and
      -- the next vertex on its other side
      TopCup -> let ([(msl1,msr1),(msl2,msr2)], mssr) = partition isConnected mss
                    (msl1',msr1',msl2',msr2') =
                      if idx v == prev (head msr1)
                      then let i = prev (head msr2)
                               v' = cvs ! i
                           in (msl1, v { prev = i } : msr1, v':v:msl2, v':msr2)
                      else let i = prev (head msr1)
                               v' = cvs ! i
                           in (msl2, v { prev = i } : msr2, v':v:msl1, v':msr1)
                in ((msl1',msr1'):mssr,(msl2',msr2'):out)
      -- close monotone segment (stage for emission, remove from
      -- active collection)
      BottomCup -> let (mss',(msl,msr):mss'') = break isConnected mss
                   in (mss' ++ mss'', (v:msl,v:msr):out)
      -- add to the segment the upper neighbour belongs to
      Side -> let (mss',(msl,msr):mss'') = break isConnected mss
                  ms' = if idx v == next (head msl) then (v:msl, msr) else (msl, v:msr)
              in (mss' ++ ms':mss'', out)
      where
        isConnected ((vl:_), (vr:_)) = idx v == next vl || idx v == prev vr
        isContained ((vl:_), (vr:_)) = px v > xl && px v <= xr
          where
            vl' = cvs ! (next vl)
            vr' = cvs ! (prev vr)
            xl = px vl + (px vl' - px vl) * (py v - py vl) / (py vl' - py vl)
            xr = px vr + (px vr' - px vr) * (py v - py vr) / (py vr' - py vr)

    scvs = V.modify (V.sortBy (comparing py)) cvs
    cvs = V.imap classify ovs
    classify i1 v1@(V x1 y1) = Vtx i1 i0 i2 vty x1 y1
      where
        vty = case (compare y1 y0, compare y1 y2, v2-v1 `turn` v1-v0) of
          (LT, LT, LT) -> BottomCap
          (EQ, LT, LT) -> BottomCap
          (LT, LT, GT) -> TopCap
          (LT, EQ, GT) -> TopCap
          (GT, GT, GT) -> BottomCup
          (EQ, GT, GT) -> BottomCup
          (GT, GT, LT) -> TopCup
          (GT, EQ, LT) -> TopCup
          _            -> Side
        i0 = if i1 == 0 then V.length ovs - 1 else i1-1
        i2 = if i1 == V.length ovs - 1 then 0 else i1+1
        v0@(V x0 y0) = ovs ! i0
        v2@(V x2 y2) = ovs ! i2

-- | Triangulation of a monotone polygon.
monotoneTriangulation :: Vector V2 -> MonotoneSegment -> [(Int,Int,Int)]
monotoneTriangulation vs (msl,msr) = snd (foldl' addVertex ([si2,si1],[]) sis)
  where
    addVertex (si@(s,i):sis,ts) si'@(s',i')
      | s /= s'   = ([si',si], zipWith (if s' then tl else tr) (si:sis) sis ++ ts)
      | concave   = (si':si:sis,ts)
      | otherwise = (si':si'':map snd si2s'', zipWith (if s' then tr else tl) sis' sis'' ++ ts)
      where
        concave = isConcave (snd (head sis)) i
        (si2s',si2s'') = break visible (zip (si:sis) sis)
          where
            visible ((_,i1),(_,i2)) = isConcave i2 i1
        (sis',sis'') = unzip si2s'
        si'' = last sis''

        tl (_,i1) (_,i2) = (i',i2,i1)
        tr (_,i1) (_,i2) = (i',i1,i2)

        isConcave i0 i1 = s' == v1-v0 `turnL` v2-v1
          where
            v0 = vs ! i0
            v1 = vs ! i1
            v2 = vs ! i'

    si1:si2:sis = merge msl (init (tail msr))
    merge [] irs = map ((,) True) irs
    merge ils [] = map ((,) False) ils
    merge ils@(il:ils') irs@(ir:irs')
      | y1 < y2   = (True,ir) : merge ils irs'
      | otherwise = (False,il) : merge ils' irs
      where
        V _ y1 = vs ! il
        V _ y2 = vs ! ir

-- | Triangulation of a simple polygon.
triangulation :: Vector V2 -> [(Int, Int, Int)]
triangulation vs = [tri | ms <- monotoneDecomposition vs, tri <- monotoneTriangulation vs ms]

-- | A triple @(d2,ds,fs)@ that describes the closest opposing
-- features (only edge-vertex pairs) of two convex polygons, where
-- @d2@ is the square of the distance, @ds@ is its sign (negative in
-- case of penetration), and @fs@ is the list describing the feature
-- pairs with this distance. Each element of @fs@ is a tuple
-- @(b,ei1,vi2,v1,v2)@ where @ei1@ is the edge id (id of its first
-- vertex counter-clockwise) and @vi2@ is the opposing vertex id, @v1@
-- and @v2@ are the absolute coordinates of the separation points
-- (@v2@ always equals the vertex), and the boolean @b@ specifies
-- whether the edge belongs to the first polygon (@True@) or the
-- second (@False@).
convexSeparations :: Vector V2    -- ^ The vertices of the first polygon (vs1)
                  -> Vector Angle -- ^ Must equal @angles vs1@
                  -> Vector V2    -- ^ The vertices of the second polygon (vs2)
                  -> Vector Angle -- ^ Must equal @angles vs2@
                  -> (Float,Float,[(Bool,Int,Int,V2,V2)])
convexSeparations vs1 as1 vs2 as2 = (d2min,-dsmin,
                                     map finaliseDist (filter ((==dmin) . getDist) cs))
  where
    cs = [(s,ei1,vi2,v1,v2,ds,d2) |
          (s,cs) <- [(True,evcs vs1 as1 vs2 as2),(False,evcs vs2 as2 vs1 as1)],
          (ei1,vi2,(v1,v2,ds,d2)) <- cs]
    dmin@(d2min,dsmin) = minimum (map getDist cs)
    -- Preferring positive distances (needed for acute corners)
    getDist (_,_,_,_,_,ds,d2) = (d2,-ds)
    finaliseDist (s,ei1,vi2,v1,v2,ds,d2) = (s,ei1,vi2,v1,v2)

    -- Matching the vertices of vs2 to the opposing edges of vs1
    evcs vs1 as1 vs2 as2 = [(ei1,vi2,separation ei1 vi2) | (vi2,as) <- matches, (ei1,a) <- as]
      where
        as1' = V.imap (,) as1
        as2' = V.map (+<pi) as2
        ivs2 = V.imap (,) (V.zip (V.cons (V.last as2') as2') as2')
        ivs2' = V.take (V.length ivs2 + 1) $
                V.dropWhile (not . opposite) $ V.dropWhile opposite $
                V.cons (V.last ivs2) ivs2 V.++ ivs2
        opposite = between (snd (V.head as1')) . snd

        -- Finding opposing edge-vertex pairs
        matches = go (V.toList ivs2') (V.toList as1')
          where
            go [] _ = []
            go ((i1,iv):ivs) as = if null pre then go ivs post
                                  else (i1,pre) : go ivs rest
              where
                (pre,post) = span ((`between` iv) . snd) as
                a = last pre
                rest = if snd iv == snd a then a : post else post

        -- Extracting separation information (coordinates and signed
        -- distance squared)
        separation ei1 vi2 = (v,v3,signum cp,d2)
          where
            v1 = vs1 ! ei1
            v2 = vs1 ! mod (ei1+1) (V.length vs1)
            v3 = vs2 ! vi2
            v12 = v2-v1
            v13 = v3-v1
            v23 = v3-v2
            sd12 = square v12
            sd12' = recip sd12
            dp = v12 `dot` v13
            cp = v13 `cross` v12
            (v,d2) = if dp <= 0 then (v1,square v13)
                     else if dp >= sd12 then (v2,square v23)
                          else (v1+v12*.(dp*sd12'),cp*cp*sd12')
