module Camera (Camera,
               Rasterer(..),
               defaultCamera,
               rasterRays) where

import Control.Monad (replicateM)
import Control.Monad.State (runState, state)
import System.Random (Random, RandomGen, random)

import Ray (Ray(..))
import Vector (Vec3(..), vec)

data Camera a = Camera { cameraLowerLeftCorner :: Vec3 a
                       , cameraHorizontal :: Vec3 a
                       , cameraVertical :: Vec3 a
                       , cameraOrigin :: Vec3 a
                       , cameraSamples :: Int
                       }

defaultCamera :: Num a => Camera a
defaultCamera = let cameraLowerLeftCorner = Vec3 (-2) (-1) (-1)
                    cameraHorizontal = Vec3 4 0 0
                    cameraVertical = Vec3 0 2 0
                    cameraOrigin = Vec3 0 0 0
                    cameraSamples = 100
                in Camera {..}

cameraRay :: Num a => Camera a -> a -> a -> Ray a
cameraRay Camera{..} u v = let origin = cameraOrigin
                               direction = cameraLowerLeftCorner + (vec u * cameraHorizontal) + (vec v * cameraVertical) - cameraOrigin
                           in Ray origin direction

-- todo: terrible name
data Rasterer = Rasterer { rastererHorizontalPixels :: Int
                         , rastererVerticalPixels :: Int
                         }

-- todo: non-empty list
-- todo: return 2d array or whatever not a list
rasterRays :: (Fractional a, Random a, RandomGen g) => Rasterer -> Camera a -> g -> [[Ray a]]
rasterRays rasterer camera gen = runRandom [rays i j | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]
    where 
      nx = rastererHorizontalPixels rasterer
      nx' = fromIntegral nx
      ny = rastererVerticalPixels rasterer
      ny' = fromIntegral ny

      -- the randomly sampling of rays for the (i, j) pixel
      rays i j = replicateM (cameraSamples camera) (cameraRay camera <$> adjust i nx' <*> adjust j ny')

      -- converts the a'th pixel (of b) into the viewport dimension (with rays randomly scattered within the pixel)
      adjust a b = let scale x = (fromIntegral a + x) / b
                   in fmap scale (state random)

      runRandom states = let (c, _) = runState (sequence states) gen in c
