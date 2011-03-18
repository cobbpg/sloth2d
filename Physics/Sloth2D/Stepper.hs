module Physics.Sloth2D.Stepper
    ( Stepper
    , stepper, advance
    , lerpFactor, timeStep
    ) where

-- | A time step manager entity to fix the time steps of the
-- simulation.
data Stepper = Stepper
    { tstep :: Float
    , tstep' :: Float
    , dtmax :: Float
    , tfrac :: Float
    }

-- | Constructing a stepper.
stepper :: Float    -- ^ Physics time step.
        -> Float    -- ^ Maximum time of physics simulated in one step.
        -> Stepper  -- ^ The stepper with the above parameters fixed.
stepper tstep dtmax = Stepper
    { tstep = tstep
    , tstep' = recip tstep
    , dtmax = dtmax
    , tfrac = 0
    }

-- | Advancing a stepper.
advance :: Float           -- ^ The amount of time elapsed since the last sampling.
        -> Stepper         -- ^ The stepper to advance.
        -> (Int, Stepper)  -- ^ The number of physics steps to take and the updated stepper.
advance dt s = (n,s')
  where
    tf = tfrac s+min dt (dtmax s)
    n = floor (tf*tstep' s)
    s' = s { tfrac = tf-fromIntegral n*tstep s}

-- | The interpolation factor of a stepper: the time fraction of the
-- frame to render between 0 and 1.  If 1, the current state of the
-- simulation must be rendered, otherwise it needs to be interpolated
-- with the previous state.
lerpFactor :: Stepper -> Float
lerpFactor s = tfrac s*tstep' s

-- | The time step associated with a stepper.
timeStep :: Stepper -> Float
timeStep = tstep
