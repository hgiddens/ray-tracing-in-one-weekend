module Main where

import Data.Foldable (traverse_)

import Lib

data Colour = Colour Int Int Int

instance Show Colour where
    show (Colour r g b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b)

main :: IO ()
main = do
  putStrLn "P3"
  putStrLn $ (show nx) ++ " " ++ (show ny)
  putStrLn "255"
  traverse_ (putStrLn . show . (uncurry pixelColour)) indices
    where
      nx = 200
      ny = 100
      indices = [(i, j) | j <- [(ny - 1), (ny - 2) .. 0], i <- [0 .. (nx - 1)]]
      rescale n d = truncate $ 255.99 * (n / d)
      pixelColour i j =
          let r = rescale (fromIntegral i) (fromIntegral nx)
              g = rescale (fromIntegral j) (fromIntegral ny)
              b = rescale 0.2 1.0
          in Colour r g b
