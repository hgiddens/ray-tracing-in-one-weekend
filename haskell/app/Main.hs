module Main where

import Control.Monad (guard)
import Control.Monad.State (State, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable (foldl1, traverse_)
import Data.Maybe (fromMaybe)
import System.Random (Random, RandomGen, newStdGen)

import Camera (Camera, Rasterer(..), defaultCamera, rasterRays)
import Colour (Colour, colour, gamma2, scaleColour)
import Ray (Ray(..))
import Prim (Hit(..), MaterialInteraction(..), Prim, hit, lambertian, metal, scatterRay, sphere)
import Vector (Vec3(..), fromVector, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = gamma2 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT a m = fromMaybe a <$> runMaybeT m

-- todo: the vector -> colour conversion is gnarly and requires the real class :(
rayColour :: (Random a, RandomGen g, RealFloat a) => Prim a -> Ray a -> State g Colour
rayColour world initialRay@(Ray _ initialDirection) = go (0 :: Int) initialRay
    where
      tMin = Just 0.001
      tMax = Nothing
      depthLimit = 50

      hitColour depth ray hitRecord = fromMaybeT (colour 0 0 0) $ do
        guard (depth < depthLimit)
        interaction <- MaybeT (scatterRay (hitMaterial hitRecord) ray hitRecord)
        c <- lift (go (depth + 1) (materialScattered interaction))
        let (Vec3 x y z) = fmap realToFrac (materialAttenuation interaction)
        pure (scaleColour x y z c)

      missColour = pure (convert background)

      convert = fromVector colour . fmap realToFrac
      background = let (Vec3 _ y _) = unit initialDirection
                       t' = 0.5 * (y + 1)
                       blue = Vec3 0.5 0.7 1.0
                       white = 1
                   in vec (1 - t') * white + (vec t') * blue

      go depth ray = maybe missColour (hitColour depth ray) (hit world ray tMin tMax)

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

      leftSphere = sphere (Vec3 (-1) 0 (-1)) 0.5 (metal (Vec3 0.8 0.8 0.8) 0.3)
      rightSphere = sphere (Vec3 1 0 (-1)) 0.5 (metal (Vec3 0.8 0.6 0.2) 1)
      middleSphere = sphere (Vec3 0 0 (-1)) 0.5 (lambertian (Vec3 0.8 0.3 0.3))
      bottomSphere = sphere (Vec3 0 (-100.5) (-1)) 100 (lambertian (Vec3 0.8 0.8 0))

      world = leftSphere <> middleSphere <> rightSphere <> bottomSphere
      mergeColours = foldl1 (<>)

      pixelFromRays rays = fmap (PixelColour . mergeColours) (traverse (rayColour world) rays)
