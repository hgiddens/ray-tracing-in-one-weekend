module Ray (Ray(..),
            pointAt) where

import Vector (Vec3, vec)

data Ray = Ray { rayOrigin :: Vec3
               , rayDirection :: Vec3
               }

pointAt :: Ray -> Float -> Vec3
pointAt (Ray a b) t = a + (vec t * b)
