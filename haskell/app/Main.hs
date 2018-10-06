module Main where

import Data.Foldable (traverse_)

import Colour (Colour, bpp8, colour)
import Ray (Ray(..))
import Prim (Hit(..), Prim, hit, sphere)
import Vector (Vec3(..), fromVector, unit, vec)

-- newtype for the show instance
newtype PixelColour = PixelColour Colour
instance Show PixelColour where
    show (PixelColour c) = let (r,g,b) = bpp8 c
                           in (show r) ++ " " ++ (show g) ++ " " ++ (show b)

-- todo: numeric limits
maxValue :: RealFloat a => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)

-- todo: the vector -> colour conversion is gnarly and requires the real class :(
-- todo: the maxValue thing also drags in real
rayColour :: (Ord a, RealFloat a) => Prim a -> Ray a -> Colour
rayColour world ray@(Ray _ direction) =
    convert $ case hit world ray 0 maxValue of
                Just (Hit _ _ n) -> 0.5 * (n + 1)
                Nothing -> background
    where
      convert = fromVector colour . fmap realToFrac
      background = let (Vec3 _ y _) = unit direction
                       t' = 0.5 * (y + 1)
                       blue = Vec3 0.5 0.7 1.0
                       white = 1
                   in vec (1 - t') * white + (vec t') * blue

main :: IO ()
main = do
  putStrLn "P3"
  putStrLn $ (show nx) ++ " " ++ (show ny)
  putStrLn "255"
  traverse_ (print . PixelColour . rayColour world . ray) indices
    where
      nx = 200
      ny = 100
      indices = [(i, j) | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]

      lowerLeft, horizontal, vertical, origin :: Vec3 Double
      lowerLeft = Vec3 (-2) (-1) (-1)
      horizontal = Vec3 4 0 0
      vertical = Vec3 0 2 0
      origin = vec 0

      world = sphere (Vec3 0 0 (-1)) 0.5 <> sphere (Vec3 0 (-100.5) (-1)) 100

      ray (i,j) =
          let i' = fromIntegral i
              u = i' / (fromIntegral nx)
              j' = fromIntegral j
              v = j' / (fromIntegral ny)
              direction = lowerLeft + (vec u * horizontal) + (vec v * vertical)
          in Ray origin direction
