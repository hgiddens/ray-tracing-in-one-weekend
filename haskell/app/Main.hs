module Main where

import Control.Monad.State (State, evalState, state)
import Data.Foldable (foldl1, traverse_)
import System.Random (Random, RandomGen, newStdGen, random)

import Camera (Camera, Rasterer(..), defaultCamera, rasterRays)
import Colour (Colour, colour, gamma2, scaleColour)
import Ray (Ray(..))
import Prim (Hit(..), Prim, hit, sphere)
import Vector (Vec3(..), fromVector, squaredLength, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = gamma2 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

randomInUnitSphere :: (Fractional a, Ord a, Random a, RandomGen g) => State g (Vec3 a)
randomInUnitSphere = do
  p <- state random
  let p' = (2 * p) - 1
  if squaredLength p' >= 1
  then randomInUnitSphere
  else pure p'

-- todo: the vector -> colour conversion is gnarly and requires the real class :(
rayColour :: (Ord a, Random a, RandomGen g, RealFloat a) => Prim a -> Ray a -> State g Colour
rayColour world ray@(Ray _ direction) = maybe missColour hitColour (hit world ray tMin tMax)
    where
      tMin = Just 0.001
      tMax = Nothing

      hitColour (Hit { hitLocation, hitNormal }) =
          do r <- randomInUnitSphere
             let target = hitLocation + hitNormal + r
                 bounceRay = Ray hitLocation (target - hitLocation)
             bounceColour <- rayColour world bounceRay
             pure (scaleColour 0.5 bounceColour)
      missColour = pure (convert background)

      convert = fromVector colour . fmap realToFrac
      background = let (Vec3 _ y _) = unit direction
                       t' = 0.5 * (y + 1)
                       blue = Vec3 0.5 0.7 1.0
                       white = 1
                   in vec (1 - t') * white + (vec t') * blue

main :: IO ()
main = do
  putStrLn "P3"
  putStrLn $ (show (rastererHorizontalPixels rasterer)) ++ " " ++ (show (rastererVerticalPixels rasterer))
  putStrLn "255"
  gen <- newStdGen
  let pixels = do
         rays <- rasterRays rasterer camera
         traverse pixelFromRays rays
  traverse_ print (evalState pixels gen)
    where
      camera = defaultCamera :: Camera Double
      rasterer = Rasterer 400 200
      world = sphere (Vec3 0 0 (-1)) 0.5 <> sphere (Vec3 0 (-100.5) (-1)) 100
      mergeColours = foldl1 (<>)

      pixelFromRays rays = fmap (PixelColour . mergeColours) (traverse (rayColour world) rays)
