module Ray (Ray(..),
            pointAt) where

import Vector (Vec3)

data Ray a = Ray { rayOrigin :: Vec3 a
                 , rayDirection :: Vec3 a
                 }

pointAt :: Num a => Ray a -> a -> Vec3 a
pointAt (Ray a b) t = a + (pure t * b)
