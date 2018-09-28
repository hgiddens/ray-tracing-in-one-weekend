module Ray (Ray(..),
            pointAt) where

import Vector (Vec3)

data Ray a = Ray (Vec3 a) (Vec3 a)

pointAt :: Num a => Ray a -> a -> Vec3 a
pointAt (Ray a b) t = a + (pure t * b)
