{-# LANGUAGE TypeFamilies #-} -- why can't i turn this on everywhere?

module Vector (Vec3(..),
               cross,
               dot,
               reverseVector,
               squaredLength,
               unit,
               vec,
               vectorLength) where

import Control.Monad.State (runState, state)
import Data.MonoTraversable (Element, MonoFoldable, MonoFunctor, ofoldMap, ofoldl', ofoldl1Ex', ofoldr, ofoldr1Ex, omap, osum)
import System.Random (Random, random, randomR)

data Vec3 = Vec3 !Float !Float !Float

type instance Element Vec3 = Float

instance Num Vec3 where
    (+) = liftV2 (+)
    (-) = liftV2 (-)
    (*) = liftV2 (*)
    abs = omap abs
    negate = omap negate
    signum = omap signum
    fromInteger = vec . fromInteger

instance Fractional Vec3 where
    (/) = liftV2 (/)
    fromRational = vec . fromRational

instance MonoFoldable Vec3 where
    ofoldMap f (Vec3 x y z) = f x <> f y <> f z
    ofoldl' f a (Vec3 x y z) = a `f` x `f` y `f` z
    ofoldl1Ex' f (Vec3 x y z) = x `f` y `f` z
    ofoldr f a (Vec3 x y z) = f x $ f y $ f z a
    ofoldr1Ex f (Vec3 x y z) = f x $ f y z

instance MonoFunctor Vec3 where
    omap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Random Vec3 where
    randomR (Vec3 loX loY loZ, Vec3 hiX hiY hiZ) =
        let x = state (randomR (loX, hiX))
            y = state (randomR (loY, hiY))
            z = state (randomR (loZ, hiZ))
        in runState (Vec3 <$> x <*> y <*> z)
    random = runState (Vec3 <$> state random <*> state random <*> state random)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x y z) (Vec3 x' y' z') =
    let x'' = (y * z') - (z * y')
        y'' = negate $ (x * z') - (z * x')
        z'' = (x * y') - (y * x')
    in Vec3 x'' y'' z''

dot :: Vec3 -> Vec3 -> Float
dot a b = osum $ a * b

liftV2 :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
liftV2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')

reverseVector :: Vec3 -> Vec3
reverseVector = omap negate

squaredLength :: Vec3 -> Float
squaredLength v = dot v v

unit :: Vec3 -> Vec3
unit v = let l = vectorLength v in omap (/l) v

vec :: Float -> Vec3
vec a = Vec3 a a a

vectorLength :: Vec3 -> Float
vectorLength = sqrt . squaredLength

