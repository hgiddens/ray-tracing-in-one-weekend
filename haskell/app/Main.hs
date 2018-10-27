module Main where

import Data.Foldable (foldl1, traverse_)
import System.Random (newStdGen)

import Camera (Camera, Rasterer(..), defaultCamera, rasterRays)
import Colour (Colour, bpp8, colour)
import Ray (Ray(..))
import Prim (Hit(..), Prim, hit, sphere)
import Vector (Vec3(..), fromVector, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = bpp8 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

-- todo: the vector -> colour conversion is gnarly and requires the real class :(
rayColour :: (Ord a, RealFloat a) => Prim a -> Ray a -> Colour
rayColour world ray@(Ray _ direction) =
    convert $ case hit world ray tMin tMax of
                Just (Hit {hitNormal}) -> 0.5 * (hitNormal + 1)
                Nothing -> background
    where
      tMin = Just 0
      tMax = Nothing
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
  let rays = rasterRays rasterer camera gen
  traverse_ (print . PixelColour . mergeColours . fmap (rayColour world)) rays
    where
      camera = defaultCamera :: Camera Double
      rasterer = Rasterer 400 200
      world = sphere (Vec3 0 0 (-1)) 0.5 <> sphere (Vec3 0 (-100.5) (-1)) 100
      mergeColours = foldl1 (<>)
