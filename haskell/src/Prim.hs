module Prim (Hit(..),
             Prim,
             hit,
             sphere) where

import Data.Foldable (foldl')

import Ray (Ray(..), pointAt)
import Vector (Vec3, dot, vec)

-- todo: fancy records
data Hit a = Hit a (Vec3 a) (Vec3 a)

-- todo: deriving
newtype Prim a = Prim [Ray a -> a -> a -> Maybe (Hit a)]
instance Semigroup (Prim a) where
    (Prim a) <> (Prim b) = Prim (a <> b)
instance Monoid (Prim a) where
    mempty = Prim mempty

-- todo: should map the hit tests then find min of hits?
hit :: Prim a -> Ray a -> a -> a -> Maybe (Hit a)
hit (Prim prims) ray tMin tMax = foldl' firstHit Nothing prims
    where
      firstHit Nothing prim = prim ray tMin tMax
      firstHit h@(Just (Hit closestSoFar _ _)) prim =
          case prim ray tMin closestSoFar of Nothing -> h
                                             Just h' -> Just h'
      

sphere :: (Floating a, Ord a) => Vec3 a -> a -> Prim a
sphere centre radius = Prim [hitSphere centre radius]
    
hitSphere :: (Floating a, Ord a) => Vec3 a -> a -> Ray a -> a -> a -> Maybe (Hit a)
hitSphere centre radius ray@(Ray origin direction) tMin tMax
    | discriminant <= 0 = Nothing
    | firstRoot < tMax && firstRoot > tMin = Just (hit firstRoot)
    | secondRoot < tMax && secondRoot > tMin = Just (hit secondRoot)
    | otherwise = Nothing
    where oc = origin - centre
          a = dot direction direction
          b = dot oc direction
          c = dot oc oc - radius * radius
          discriminant = b * b - a * c
          firstRoot = (-b - sqrt discriminant) / a
          secondRoot = (-b + sqrt discriminant) / a
          hit t = let p = (pointAt ray t)
                  in Hit t p ((p - centre) / vec radius)
