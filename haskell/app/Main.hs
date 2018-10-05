module Main where

import Data.Foldable (traverse_)

import Colour (Colour, bpp8, colour)
import Ray (Ray(..), pointAt)
import Vector (Vec3(..), dot, fromVector, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = bpp8 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

hitSphere :: (Floating a, Ord a) => Vec3 a -> a -> Ray a -> a
hitSphere centre radius (Ray origin direction) =
    let oc = origin - centre
        a = dot direction direction
        b = 2 * dot oc direction
        c = dot oc oc - radius * radius
        discriminant = b * b - 4 * a * c
    in if discriminant < 0
       then -1
       else (-b - sqrt discriminant) / (2 * a)

-- todo: the vector -> colour conversion is gnarly and requires the real class :(
rayColour :: (Floating a, Ord a, Real a) => Ray a -> Colour
rayColour ray@(Ray _ direction) =
    let centre = Vec3 0 0 (-1)
        blue = Vec3 0.5 0.7 1.0
        white = 1
        radius = 0.5
        t = hitSphere centre radius ray
        convert = fromVector colour . fmap realToFrac
        background = let (Vec3 _ y _) = unit direction
                         t' = 0.5 * (y + 1)
                     in vec (1 - t') * white + (vec t') * blue
        normal = let n = unit (pointAt ray t - centre)
                 in 0.5 * (n + 1)
    in convert (if t > 0 then normal else background)

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
              direction = lowerLeft + (vec u * horizontal) + (vec v * vertical)
          in Ray origin direction
