module Vector (Vec3(..),
               cross,
               dot,
               reverseVector,
               squaredLength,
               unit,
               vec,
               vectorLength) where

import Control.Monad.State (runState, state)
import System.Random (Random, random, randomR)

data Vec3 = Vec3 Float Float Float

-- todo: monotraversible

liftV2 :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
liftV2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')

vmap :: (Float -> Float) -> Vec3 -> Vec3
vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Num Vec3 where
    (+) = liftV2 (+)
    (-) = liftV2 (-)
    (*) = liftV2 (*)
    abs = vmap abs
    negate = vmap negate
    signum = vmap signum
    fromInteger = vec . fromInteger

instance Fractional Vec3 where
    (/) = liftV2 (/)
    fromRational = vec . fromRational

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
dot (Vec3 x y z) (Vec3 x' y' z') = (x * x') + (y * y') + (z * z')

reverseVector :: Vec3 -> Vec3
reverseVector = vmap negate

squaredLength :: Vec3 -> Float
squaredLength v = dot v v

unit :: Vec3 -> Vec3
unit v = let l = vectorLength v in vmap (/l) v

vec :: Float -> Vec3
vec a = Vec3 a a a

vectorLength :: Vec3 -> Float
vectorLength = sqrt . squaredLength

