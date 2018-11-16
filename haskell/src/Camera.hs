module Camera (Camera,
               Viewport(..),
               camera,
               viewportRays) where

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
                     , cameraLensRadius :: Float
                     }

-- todo: it would be nice to find a way of writing these that isn't so memory heavy
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
        cameraLensRadius = aperture / 2
    in Camera {..}

randomInUnitDisk :: RandomGen g => State g Vec3
randomInUnitDisk = do
  p <- fromXY <$> state random <*> state random
  if ok p then pure p else randomInUnitDisk
      where fromXY x y = Vec3 (2 * x) (2 * y) 0 - Vec3 1 1 0
            ok p = dot p p < 1

cameraRay :: RandomGen g => Camera -> Float -> Float -> State g Ray
cameraRay Camera{..} s t = fmap (withOrigin . toOrigin) randomInUnitDisk
    where withOrigin origin = let direction = cameraLowerLeftCorner + (vec s * cameraHorizontal) + (vec t * cameraVertical) - origin
                              in Ray origin direction
          toOrigin r = let Vec3 x y _ = vec cameraLensRadius * r
                       in cameraOrigin + ((vec x * cameraU) + (vec y * cameraV))

data Viewport = Viewport { viewportHorizontalPixels :: Int
                         , viewportVerticalPixels :: Int
                         }

-- todo: return 2d array or whatever not a list, or a non-empty list maybe?
viewportRays :: RandomGen g => Viewport -> Camera -> Int -> State g [[Ray]]
viewportRays viewport c ns = sequence [rays i j | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]
    where 
      nx = viewportHorizontalPixels viewport
      nx' = fromIntegral nx
      ny = viewportVerticalPixels viewport
      ny' = fromIntegral ny

      -- the random sampling of rays for the (i, j) pixel
      rays i j = let randomRay = do i' <- adjust i nx'
                                    j' <- adjust j ny'
                                    cameraRay c i' j'
                 in replicateM ns randomRay

      -- converts the a'th pixel (of b) into the viewport dimension (with rays randomly scattered within the pixel)
      adjust a b = let scale x = (fromIntegral a + x) / b
                   in fmap scale (state random)
