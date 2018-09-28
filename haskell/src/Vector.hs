module Vector (Vec3(..),
               cross,
               dot,
               scale,
               squaredLength,
               unit,
               vec,
               vectorLength) where

import Control.Applicative (liftA2)

data Vec3 a = Vec3 a a a

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    liftA2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')
    pure a = Vec3 a a a

instance Num a => Num (Vec3 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger


instance Fractional a => Fractional (Vec3 a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x y z) (Vec3 x' y' z') =
    let x'' = (y * z') - (z * y')
        y'' = negate $ (x * z') - (z * x')
        z'' = (x * y') - (y * x')
    in Vec3 x'' y'' z''

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x y z) (Vec3 x' y' z') = (x * x') + (y * y') + (z * z')

scale :: Num a => a -> Vec3 a -> Vec3 a
scale t = fmap (t*)

squaredLength :: Num a => Vec3 a -> a
squaredLength v = dot v v

unit :: Floating a => Vec3 a -> Vec3 a
unit v = let l = vectorLength v in fmap (/l) v

vec :: a -> Vec3 a
vec = pure

vectorLength :: Floating a => Vec3 a -> a
vectorLength = sqrt . squaredLength

