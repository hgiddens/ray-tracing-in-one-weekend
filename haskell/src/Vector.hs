module Vector (Vec3(..),
               cross,
               dot,
               fromVector,
               reverseVector,
               squaredLength,
               unit,
               vec,
               vectorLength) where

import Control.Applicative (liftA2)
import Control.Monad.State (runState, state)
import System.Random (Random, random, randomR)

data Vec3 a = Vec3 a a a deriving (Functor, Foldable)

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

instance Random a => Random (Vec3 a) where
    randomR (Vec3 loX loY loZ, Vec3 hiX hiY hiZ) =
        let x = state (randomR (loX, hiX))
            y = state (randomR (loY, hiY))
            z = state (randomR (loZ, hiZ))
        in runState (Vec3 <$> x <*> y <*> z)
    random = runState (Vec3 <$> state random <*> state random <*> state random)

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x y z) (Vec3 x' y' z') =
    let x'' = (y * z') - (z * y')
        y'' = negate $ (x * z') - (z * x')
        z'' = (x * y') - (y * x')
    in Vec3 x'' y'' z''

dot :: Num a => Vec3 a -> Vec3 a -> a
dot a b = sum (a * b)

fromVector :: (a -> a -> a -> b) -> Vec3 a -> b
fromVector f (Vec3 x y z) = f x y z

reverseVector :: Num a => Vec3 a -> Vec3 a
reverseVector = fmap negate

squaredLength :: Num a => Vec3 a -> a
squaredLength v = dot v v

unit :: Floating a => Vec3 a -> Vec3 a
unit v = let l = vectorLength v in fmap (/l) v

vec :: a -> Vec3 a
vec = pure

vectorLength :: Floating a => Vec3 a -> a
vectorLength = sqrt . squaredLength

