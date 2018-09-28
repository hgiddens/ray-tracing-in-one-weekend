module Main where

import Data.Foldable (traverse_)

import Colour (Colour, bpp8, colour)
import Ray (Ray(..))
import Vector (Vec3(..), scale, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = bpp8 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

main :: IO ()
main = do
  putStrLn "P3"
  putStrLn $ (show nx) ++ " " ++ (show ny)
  putStrLn "255"
  traverse_ (print . rayColour . ray) indices
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

      rayColour (Ray _ direction) =
          let (Vec3 _ y _) = unit direction
              t = 0.5 * (y + 1.0)
              (Vec3 r g b) = scale (1.0 - t) (vec 1.0) + scale t (Vec3 0.5 0.7 1.0)
          in PixelColour $ colour r g b
