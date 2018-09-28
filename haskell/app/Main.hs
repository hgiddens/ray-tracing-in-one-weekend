module Main where

import Data.Foldable (traverse_)

import Colour (Colour, bpp8, colour)
import Ray (Ray(..))
import Vector (Vec3(..), dot, scale, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = bpp8 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

hitSphere :: (Num a, Ord a) => Vec3 a -> a -> Ray a -> Bool
hitSphere centre radius (Ray origin direction) =
    let oc = origin - centre
        a = dot direction direction
        b = 2 * (dot oc direction)
        c = (dot oc oc) - (radius * radius)
        discriminant = (b * b) - (4 * a * c)
    in discriminant > 0

-- todo: the vector -> colour conversion is gnarly and requires the real class :(
rayColour :: (Real a, Floating a, Ord a) => Ray a -> Colour
rayColour ray@(Ray _ direction) | hitSphere centre radius ray = red
                                | otherwise = background
    where
      centre = Vec3 0 0 (-1)
      radius = 0.5
      red = colour 1 0 0
      background = let (Vec3 _ y _) = unit direction
                       t = 0.5 * (y + 1.0)
                       (Vec3 r g b) = scale (1.0 - t) (vec 1.0) + scale t (Vec3 0.5 0.7 1.0)
                   in colour (realToFrac r) (realToFrac g) (realToFrac b)

main :: IO ()
main = do
  putStrLn "P3"
  putStrLn $ (show nx) ++ " " ++ (show ny)
  putStrLn "255"
  traverse_ (print . PixelColour . rayColour . ray) indices
    where
      nx = 200
      ny = 100
      indices = [(i, j) | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]

      lowerLeft, horizontal, vertical, origin :: Vec3 Double
      lowerLeft = Vec3 (-2) (-1) (-1)
      horizontal = Vec3 4 0 0
      vertical = Vec3 0 2 0
      origin = vec 0

      ray (i,j) =
          let i' = fromIntegral i
              u = i' / (fromIntegral nx)
              j' = fromIntegral j
              v = j' / (fromIntegral ny)
              direction = lowerLeft + (scale u horizontal) + (scale v vertical)
          in Ray origin direction
