module Vector (Vec3(..),
               cross,
               dot,
               vectorLength,
               squaredLength,
               unit) where

data Vec3 a = Vec3 a a a

instance Num a => Num (Vec3 a) where
    (Vec3 x y z) + (Vec3 x' y' z') = Vec3 (x + x') (y + y') (z + z')
    (Vec3 x y z) - (Vec3 x' y' z') = Vec3 (x - x') (y - y') (z - z')
    (Vec3 x y z) * (Vec3 x' y' z') = Vec3 (x * x') (y * y') (z * z')
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    negate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
    fromInteger i = let i' = fromInteger i in Vec3 i' i' i'


instance Fractional a => Fractional (Vec3 a) where
    (Vec3 x y z) / (Vec3 x' y' z') = Vec3 (x / x') (y / y') (z / z')
    fromRational r = let r' = fromRational r in Vec3 r' r' r'

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x y z) (Vec3 x' y' z') =
    let x'' = (y * z') - (z * y')
        y'' = negate $ (x * z') - (z * x')
        z'' = (x * y') - (y * x')
    in Vec3 x'' y'' z''

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x y z) (Vec3 x' y' z') = (x * x') + (y * y') + (z * z')

vectorLength :: Floating a => Vec3 a -> a
vectorLength = sqrt . squaredLength

squaredLength :: Num a => Vec3 a -> a
squaredLength v = dot v v

unit :: Floating a => Vec3 a -> Vec3 a
unit v@(Vec3 x y z) = let l = vectorLength v
                      in Vec3 (x/l) (y/l) (z/l)
