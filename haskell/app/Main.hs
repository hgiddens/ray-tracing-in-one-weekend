module Main where

import Data.Foldable (traverse_)

import Colour (Colour, bpp8, colour)

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
  traverse_ (putStrLn . show . pixelColour) indices
    where
      nx = 200
      ny = 100
      indices = [(i, j) | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]
      pixelColour (i,j) =
          let r = (fromIntegral i) / (fromIntegral nx)
              g = (fromIntegral j) / (fromIntegral ny)
              b = 0.2
          in PixelColour $ colour r g b
