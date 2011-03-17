import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.List
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL as GL
import System.IO

import Physics.Sloth2D.Body
import Physics.Sloth2D.Shape

import Physics.Sloth2D.Geometry2D
import Physics.Sloth2D.Vector2D

bodies =
    [ fromShape (regularShape 3 1) `withMass` 1 `withPosition` (V 0.5 2, 0) `withVelocity` (V 0.5 0.1, 0)
    , fromShape (regularShape 4 1) `withMass` 2 `withPosition` (V (-2) 2, pi/8) `withVelocity` (V 1 0, 0)
    , fromShape (regularShape 5 1) `withMass` 3 `withPosition` (V 2 1, 0) `withVelocity` (V 0 0, 1)
    ]

main = do
    initialize
    openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
    windowTitle $= "Sloth2D test"
    closed <- newIORef False
    windowCloseCallback $= writeIORef closed True
    windowSizeCallback $= \size@(Size w h) -> do
        let r = (fromIntegral h/fromIntegral w)
            r' = recip r
            lr = 9/16
            lr' = recip lr
            s = 2*min (max 1 (min lr' r')) (max (r*lr') lr')
        viewport $= (Position 0 0,size)
        matrixMode $= Projection
        loadIdentity
        GL.scale (s*min 1 r) (s*min 1 r') (1 :: GLfloat)
        GL.translate $ Vector3 0 (-0.5*lr) (0 :: GLfloat)

    bodyLists <- forM bodies $ \body -> do
        let sh = shape body
            vs = vertices sh
            tris = triangulation vs

        defineNewList Compile $ renderPrimitive Triangles $
            forM_ tris $ \(i1,i2,i3) -> do
                let V x1 y1 = vs ! i1
                    V x2 y2 = vs ! i2
                    V x3 y3 = vs ! i3
                vertex $ Vertex3 (realToFrac x1) (realToFrac y1) (0 :: GLfloat)
                vertex $ Vertex3 (realToFrac x2) (realToFrac y2) (0 :: GLfloat)
                vertex $ Vertex3 (realToFrac x3) (realToFrac y3) (0 :: GLfloat)

    flip fix bodies $ \loop bodies -> do
        render bodies bodyLists
        sleep 0.02
        stop <- readIORef closed
        esc <- getKey ESC
        when (not stop && esc /= Press) (loop (advance 0.02 bodies))

    closeWindow

render bodies lists = do
    clear [ColorBuffer]

    let magn = recip 10
    matrixMode $= Modelview 0
    loadIdentity
    GL.scale magn magn (1 :: GLfloat)

    color $ Color4 1 1 1 (1 :: GLfloat)
    forM_ (zip bodies lists) $ \(body,list) -> preservingMatrix $ do
        let V x y = curP body
            a = curA body
        GL.translate $ Vector3 (realToFrac x) (realToFrac y) (0 :: GLfloat)
        GL.rotate (realToFrac (a*180/pi) :: GLfloat) $ Vector3 0 0 1
        callList list

    forM_ [(b,b') | (b:bs) <- tails bodies, b' <- bs] $ \(b1,b2) -> do
        let (vs1,as1) = curGeometry b1
            (vs2,as2) = curGeometry b2
            (_,ds,fs) = convexSeparations vs1 as1 vs2 as2
        color $ if ds < 0 then Color4 1 0 0 1 else Color4 0 1 0 (1 :: GLfloat)
        renderPrimitive Lines $ forM_ fs $ \(_,_,_,V x1 y1,V x2 y2) -> do
            vertex $ Vertex3 (realToFrac x1) (realToFrac y1) (0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x2) (realToFrac y2) (0 :: GLfloat)

    flush
    swapBuffers

regularShape n s = polygonShape (V.generate n f)
  where
    f i = unit (2*pi*fromIntegral i/fromIntegral n) *. s

advance dt bodies = V.toList (V.map (integrate dt) collbs)
  where
    num = length bodies - 1
    bs = V.map shiftBody (V.fromList bodies)
    collbs = V.accum nudgedBy bs [r | i1 <- [0..num], i2 <- [i1+1..num], r <- check i1 i2]
      where
        check i1 i2 = case collisionResponse 1 (bs ! i1) (bs ! i2) of
            Nothing -> []
            Just (v1,w1,v2,w2) -> [(i1,(v1,w1)),(i2,(v2,w2))]
