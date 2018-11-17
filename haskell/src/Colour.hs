module Colour (Colour, bpp8, colour, gamma2, scaleColour, tween) where

import Control.Monad.State.Strict (runState, state)
import System.Random (Random, random, randomR)

data Colour = Colour Float Float Float Int

instance Random Colour where
    random = let x = state $ randomR (0, 1)
             in runState $ Colour <$> x <*> x <*> x <*> pure 1

    randomR (lo, hi) = let (lr, lg, lb) = normalise lo
                           (hr, hg, hb) = normalise hi
                           r = state $ randomR (lr, hr)
                           g = state $ randomR (lg, hg)
                           b = state $ randomR (lb, hb)
                       in runState $ Colour <$> r <*> g <*> b <*> pure 1

instance Semigroup Colour where
    (Colour r g b c) <> (Colour r' g' b' c') = Colour (r + r') (g + g') (b + b') (c + c')

colour :: Float -> Float -> Float -> Colour
colour r g b | ok(r) && ok(g) && ok(b) = Colour r g b 1
             | otherwise = error "Invalid colour"
    where ok d = d >= 0 && d <= 1

bpp8 :: Colour -> (Int, Int, Int)
bpp8 (Colour r g b count) =
    let scale = fromIntegral count
        int = truncate . (255.99*)
        r' = r / scale
        g' = g / scale
        b' = b / scale
    in (int r', int g', int b')

gamma2 :: Colour -> (Int, Int, Int)
gamma2 (Colour r g b count) =
    let scale = fromIntegral count
        int = truncate . (255.99*) . sqrt
        r' = r / scale
        g' = g / scale
        b' = b / scale
    in (int r', int g', int b')

normalise :: Colour -> (Float, Float, Float)
normalise (Colour r g b 1) = (r, g, b)
normalise (Colour r g b c) = let c' = fromIntegral c
                             in ((r / c'), (g / c'), (b / c'))

scaleColour' :: Float -> Float -> Float -> Colour -> Colour
scaleColour' x y z (Colour r g b c) = Colour (r * x) (g * y) (b * z) c

scaleColour :: Colour -> Colour -> Colour
scaleColour (Colour x y z 1) = scaleColour' x y z
scaleColour (Colour x y z c) = let c' = fromIntegral c
                               in scaleColour' (x / c') (y / c') (z / c')

tween :: Float -> Colour -> Colour -> Colour
tween t from to = let (fr, fg, fb) = normalise from
                      (tr, tg, tb) = normalise to
                      combine a b = ((1 - t) * a) + (t * b)
                      result = Colour (combine fr tr) (combine fg tg) (combine fb tb) 1
                  in if t < 0 || t > 1 then error "Invalid time." else result
