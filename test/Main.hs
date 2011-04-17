import Control.Monad
import Control.Monad.Fix
import Data.IORef
import qualified Data.IntMap as M
import Data.List
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL as GL hiding (position)
import System.IO

import Physics.Sloth2D.Body
import Physics.Sloth2D.Dynamics
import Physics.Sloth2D.Shape
import Physics.Sloth2D.Stepper

import Physics.Sloth2D.Geometry2D
import Physics.Sloth2D.Vector2D

(world, _) = addBodies (dynamicWorld (1/60) 0.2 (V 0 (-8))) $ map (`withElasticity` 0.1) $
    [ fromShape (regularShape 3 10) `withPosition` (V 12 0, 0)
    , fromShape (regularShape 3 10) `withPosition` (V (-12) 0, pi)
    , fromShape (regularShape 3 10) `withPosition` (V 0 10, pi*0.5)
    , fromShape (regularShape 3 10) `withPosition` (V 0 (-10), pi*1.5)
    , fromShape (regularShape 4 1) `withMass` 1 `withPosition` (V 0.5 0, 0) `withVelocity` (V 3 1, 0)
    , fromShape (regularShape 5 1) `withMass` 2 `withPosition` (V (-2) 0, pi/8) `withVelocity` (V 2 3, 0)
    , fromShape (regularShape 6 2) `withMass` 30 `withPosition` (V 2 (-1), 0) `withVelocity` (V 4 0, 1)
    ]
    ++
    [fromShape (regularShape (i `div` 3+3) 0.4) `withMass` 0.3 `withPosition` (unit a*.4, 0) `withVelocity` (unit a*.(1.5), 0) |
     i <- [0..19], let a = pi*fromIntegral i/10]

main = do
    initialize
    openWindow (Size 800 600) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
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

    bodyLists <- forM (M.elems (bodies world)) $ \body -> do
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

    blend $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
    time $= 0
    flip fix world $ \loop world -> do
        render (lerpFactor (manager world)) (M.elems (bodies world)) bodyLists
        sleep 0.02
        stop <- readIORef closed
        esc <- getKey ESC
        dt <- get time
        time $= 0
        when (not stop && esc /= Press) (loop (world `advancedBy` (realToFrac dt)))
    closeWindow

render dt bodies lists = do
    clear [ColorBuffer]

    let magn = recip 15
    matrixMode $= Modelview 0
    loadIdentity
    GL.scale magn magn (1 :: GLfloat)

    color $ Color4 1 1 1 (0.5 :: GLfloat)
    forM_ (zip bodies lists) $ \(body,list) -> preservingMatrix $ do
        let V x y = position dt body
            a = orientation dt body
        GL.translate $ Vector3 (realToFrac x) (realToFrac y) (0 :: GLfloat)
        GL.rotate (realToFrac (a*180/pi) :: GLfloat) $ Vector3 0 0 1
        callList list
{-
    forM_ [(b,b') | (b:bs) <- tails bodies, b' <- bs] $ \(b1,b2) -> do
        let (vs1,as1) = curGeometry b1
            (vs2,as2) = curGeometry b2
            (_,ds,fs) = convexSeparations vs1 as1 vs2 as2
        color $ if ds < 0 then Color4 1 0 0 1 else Color4 0 1 0 (1 :: GLfloat)
        renderPrimitive Lines $ forM_ fs $ \(_,_,_,V x1 y1,V x2 y2) -> do
            vertex $ Vertex3 (realToFrac x1) (realToFrac y1) (0 :: GLfloat)
            vertex $ Vertex3 (realToFrac x2) (realToFrac y2) (0 :: GLfloat)
-}
    flush
    swapBuffers

regularShape n s = polygonShape (V.generate n f)
  where
    f i = unit (2*pi*fromIntegral i/fromIntegral n) *. s
