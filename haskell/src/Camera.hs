module Camera (Camera,
               Rasterer(..),
               camera,
               rasterRays) where

import Control.Monad (replicateM)
import Control.Monad.State (State, state)
import System.Random (RandomGen, random)

import Ray (Ray(..))
import Vector (Vec3(..), cross, dot, unit, vec)

data Camera = Camera { cameraLowerLeftCorner :: Vec3
                     , cameraHorizontal :: Vec3
                     , cameraVertical :: Vec3
                     , cameraOrigin :: Vec3
                     , cameraU :: Vec3
                     , cameraV :: Vec3
                     , cameraW :: Vec3
                     , cameraLensRadius :: Float
                     , cameraSamples :: Int -- todo: this has no business here
                     }

defaultSamples = 100
defaultSamples :: Int

camera :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Float -> Camera
camera lookfrom lookat up vfov aspect aperture focusDist =
    let theta = vfov * pi / 180
        halfHeight = tan (theta / 2)
        halfWidth = aspect * halfHeight
        w = unit (lookfrom - lookat)
        u = unit (cross up w)
        v = cross w u
        cameraOrigin = lookfrom
        cameraLowerLeftCorner = cameraOrigin - (vec (halfWidth * focusDist) * u) - (vec (halfHeight * focusDist) * v) - (vec focusDist * w)
        cameraHorizontal = vec (2 * halfWidth * focusDist) * u
        cameraVertical = vec (2 * halfHeight * focusDist) * v
        cameraU = u
        cameraV = v
        cameraW = w
        cameraLensRadius = aperture / 2
        cameraSamples = defaultSamples
    in Camera {..}

-- the fractional class is mostly for the [0, 1) bound
-- todo: it would be nice to find a way of writing these that isn't so memory heavy
randomInUnitDisk :: RandomGen g => State g Vec3
randomInUnitDisk = do
  p <- fromXY <$> state random <*> state random
  if ok p then pure p else randomInUnitDisk
      where fromXY x y = Vec3 (2 * x) (2 * y) 0 - Vec3 1 1 0
            ok p = dot p p < 1

cameraRay :: RandomGen g => Camera -> Float -> Float -> State g Ray
cameraRay Camera{..} s t = fmap withOffset randomInUnitDisk
    where withOffset r = let Vec3 x y _ = vec cameraLensRadius * r
                             offset = (vec x * cameraU) + (vec y * cameraV)
                             origin = cameraOrigin + offset
                             direction = cameraLowerLeftCorner + (vec s * cameraHorizontal) + (vec t * cameraVertical) - cameraOrigin - offset
                         in Ray origin direction

-- todo: terrible name; viewport?
data Rasterer = Rasterer { rastererHorizontalPixels :: Int
                         , rastererVerticalPixels :: Int
                         }

-- todo: non-empty list
-- todo: return 2d array or whatever not a list
rasterRays :: RandomGen g => Rasterer -> Camera -> State g [[Ray]]
rasterRays rasterer c = sequence [rays i j | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]
    where 
      nx = rastererHorizontalPixels rasterer
      nx' = fromIntegral nx
      ny = rastererVerticalPixels rasterer
      ny' = fromIntegral ny

      -- the randomly sampling of rays for the (i, j) pixel
      rays i j = let randomRay = do i' <- adjust i nx'
                                    j' <- adjust j ny'
                                    cameraRay c i' j'
                 in replicateM (cameraSamples c) randomRay

      -- converts the a'th pixel (of b) into the viewport dimension (with rays randomly scattered within the pixel)
      adjust a b = let scale x = (fromIntegral a + x) / b
                   in fmap scale (state random)
