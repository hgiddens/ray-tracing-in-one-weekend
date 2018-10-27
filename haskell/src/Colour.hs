module Colour (Colour, bpp8, colour) where

data Colour = Colour Double Double Double Int

instance Semigroup Colour where
    (Colour r g b c) <> (Colour r' g' b' c') = Colour (r + r') (g + g') (b + b') (c + c')

-- TODO: error
colour :: Double -> Double -> Double -> Colour
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
