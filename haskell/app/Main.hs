module Main where

import Control.Monad (guard)
-- todo: i think this being lazy is why we need #all the memory
import Control.Monad.State (State, evalState, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable (foldl1, traverse_)
import Data.Maybe (fromMaybe)
import System.Random (RandomGen, mkStdGen, random)

import Camera (Viewport(..), camera, viewportRays)
import Colour (Colour, colour, gamma2, scaleColour, tween)
import Ray (Ray(..))
import Prim (Hit(..), MaterialInteraction(..), Prim, dielectric, hit, lambertian, metal, scatterRay, sphere)
import Vector (Vec3(..), unit, vectorLength)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = gamma2 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT a m = fromMaybe a <$> runMaybeT m

rayColour :: RandomGen g => Prim -> Ray -> State g Colour
rayColour world initialRay@(Ray _ initialDirection) = go (0 :: Int) initialRay
    where
      tMin = Just 0.001
      tMax = Nothing
      depthLimit = 50

      hitColour depth ray hitRecord = fromMaybeT (colour 0 0 0) $ do
        guard (depth < depthLimit)
        interaction <- MaybeT (scatterRay (hitMaterial hitRecord) ray hitRecord)
        c <- lift (go (depth + 1) (materialScattered interaction))
        let v = materialAttenuation interaction
        pure $ scaleColour v c

      missColour = pure background

      background = let (Vec3 _ y _) = unit initialDirection
                       t = 0.5 * (y + 1)
                       blue = colour 0.5 0.7 1
                       white = colour 1 1 1
                   in tween t white blue

      go depth ray = maybe missColour (hitColour depth ray) (hit world ray tMin tMax)

randomWorld :: RandomGen g => State g Prim
randomWorld = fmap (base <>) randomSpheres
    where
      base = (sphere (Vec3 0 (-1000) 0) 1000 (lambertian (colour 0.5 0.5 0.5))) <>
             (sphere (Vec3 0 1 0) 1 (dielectric 1.5)) <>
             (sphere (Vec3 (-4) 1 0) 1 (lambertian (colour 0.4 0.2 0.1))) <>
             (sphere (Vec3 4 1 0) 1 (metal (colour 0.7 0.4 0.5) 0))

      diffuse = fmap lambertian (scaleColour <$> state random <*> state random)

      metal' = do let rescale x = 0.5 * (1 + x)
                  r <- rescale <$> state random
                  g <- rescale <$> state random
                  b <- rescale <$> state random
                  fuzz <- (*0.5) <$> state random
                  pure (metal (colour r g b) fuzz)

      glass = pure (dielectric 1.5)

      randomMat = let go r | r < 0.8 = diffuse
                           | r < 0.95 = metal'
                           | otherwise = glass
                  in state (random :: RandomGen g => g -> (Float, g)) >>= go

      randomCentre a b = do x <- state random
                            let x' = a + (0.9 * x)
                            z <- state random
                            let z' = b + (0.9 * z)
                            pure (Vec3 x' 0.2 z')

      valid centre = vectorLength (centre - Vec3 4 0.2 0) > 0.9

      mkSphere centre = sphere centre 0.2 <$> randomMat

      randomSpheres =
          do centres <- sequence [randomCentre a b | a <- [-11 .. 10], b <- [-11 .. 10]]
             let validCentres = filter valid centres
             prims <- traverse mkSphere validCentres
             pure (mconcat prims)


main :: IO ()
main = do
  putStrLn "P3"
  putStrLn $ (show (viewportHorizontalPixels viewport)) ++ " " ++ (show (viewportVerticalPixels viewport))
  putStrLn "255"
  let gen = mkStdGen 113
      pixels = do
         world <- randomWorld
         rays <- viewportRays viewport c ns
         traverse (pixelFromRays world) rays
  traverse_ print (evalState pixels gen)
    where
      ns, nx, ny :: Num a => a
      ns = 10 -- samples
      nx = 400
      ny = 200
      c = let lookFrom = Vec3 5 2 10
              lookAt = Vec3 0 1 (-1)
              up = Vec3 0 1 0
              vfov = 20
              aspect = nx / ny
              aperture = 1
              focusDistance = vectorLength (lookFrom - lookAt)
          in camera lookFrom lookAt up vfov aspect aperture focusDistance
      viewport = Viewport nx ny

      mergeColours = foldl1 (<>)

      pixelFromRays world rays = fmap (PixelColour . mergeColours) (traverse (rayColour world) rays)
