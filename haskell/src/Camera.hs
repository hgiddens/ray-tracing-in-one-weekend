module Camera (Camera,
               Rasterer(..),
               camera,
               defaultCamera,
               rasterRays) where

import Control.Monad (replicateM)
import Control.Monad.State (State, state)
import System.Random (Random, RandomGen, random)

import Ray (Ray(..))
import Vector (Vec3(..), cross, unit, vec)

data Camera a = Camera { cameraLowerLeftCorner :: Vec3 a
                       , cameraHorizontal :: Vec3 a
                       , cameraVertical :: Vec3 a
                       , cameraOrigin :: Vec3 a
                       , cameraSamples :: Int -- todo: this has no business here
                       }

defaultSamples :: Num a => a
defaultSamples = 10

defaultCamera :: Num a => Camera a
defaultCamera = let cameraLowerLeftCorner = Vec3 (-2) (-1) (-1)
                    cameraHorizontal = Vec3 4 0 0
                    cameraVertical = Vec3 0 2 0
                    cameraOrigin = Vec3 0 0 0
                    cameraSamples = defaultSamples
                in Camera {..}

camera :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> a -> a -> Camera a
camera lookfrom lookat up vfov aspect =
    let theta = vfov * pi / 180
        halfHeight = tan (theta / 2)
        halfWidth = aspect * halfHeight
        w = unit (lookfrom - lookat)
        u = unit (cross up w)
        v = cross w u
        cameraOrigin = lookfrom
        cameraLowerLeftCorner = cameraOrigin - (pure halfWidth * u) - (pure halfHeight * v) - w
        cameraHorizontal = pure (2 * halfWidth) * u
        cameraVertical = pure (2 * halfHeight) * v
        cameraSamples = defaultSamples
    in Camera {..}

cameraRay :: Num a => Camera a -> a -> a -> Ray a
cameraRay Camera{..} u v = let origin = cameraOrigin
                               direction = cameraLowerLeftCorner + (vec u * cameraHorizontal) + (vec v * cameraVertical) - cameraOrigin
                           in Ray origin direction

-- todo: terrible name; viewport?
data Rasterer = Rasterer { rastererHorizontalPixels :: Int
                         , rastererVerticalPixels :: Int
                         }

-- todo: non-empty list
-- todo: return 2d array or whatever not a list
rasterRays :: (Fractional a, Random a, RandomGen g) => Rasterer -> Camera a -> State g [[Ray a]]
rasterRays rasterer c = sequence [rays i j | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]
    where 
      nx = rastererHorizontalPixels rasterer
      nx' = fromIntegral nx
      ny = rastererVerticalPixels rasterer
      ny' = fromIntegral ny

      -- the randomly sampling of rays for the (i, j) pixel
      rays i j = replicateM (cameraSamples c) (cameraRay c <$> adjust i nx' <*> adjust j ny')

      -- converts the a'th pixel (of b) into the viewport dimension (with rays randomly scattered within the pixel)
      adjust a b = let scale x = (fromIntegral a + x) / b
                   in fmap scale (state random)
