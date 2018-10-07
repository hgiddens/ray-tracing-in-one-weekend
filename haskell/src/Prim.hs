{-# LANGUAGE ScopedTypeVariables #-}

module Prim (Hit(..),
             Prim,
             hit,
             sphere) where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Maybe (maybeToList)

import Ray (Ray(..), pointAt)
import Vector (Vec3, dot, vec)

data Hit a = Hit { hitTime :: a
                 , hitLocation :: Vec3 a
                 , hitNormal :: Vec3 a -- unit vector
                 }

type HitFn a = Ray a -> Maybe a -> Maybe a -> Maybe (Hit a)
newtype Prim a = Prim [HitFn a] deriving (Semigroup, Monoid)

hit :: Ord a => Prim a -> Ray a -> Maybe a -> Maybe a -> Maybe (Hit a)
hit (Prim prims) ray tMin tMax = 
    let testHit prim = prim ray tMin tMax
        hits = prims >>= maybeToList . testHit
    in case hits of [] -> Nothing
                    _ -> Just (minimumBy (compare `on` hitTime) hits)

sphere :: (Floating a, Ord a) => Vec3 a -> a -> Prim a
sphere centre radius = Prim [hitSphere centre radius]
    
hitSphere :: (Floating a, Ord a) => Vec3 a -> a -> Ray a -> Maybe a -> Maybe a -> Maybe (Hit a)
hitSphere centre radius ray@(Ray origin direction) tMin tMax
    | discriminant <= 0 = Nothing
    | inBounds firstRoot = Just (hitAt firstRoot)
    | inBounds secondRoot = Just (hitAt secondRoot)
    | otherwise = Nothing
    where oc = origin - centre
          a = dot direction direction
          b = dot oc direction
          c = dot oc oc - radius * radius
          discriminant = b * b - a * c
          firstRoot = (-b - sqrt discriminant) / a
          secondRoot = (-b + sqrt discriminant) / a
          hitAt t = let p = (pointAt ray t)
                    in Hit t p ((p - centre) / vec radius)
          inBounds root = let checkMin = maybe True (root >) tMin
                              checkMax = maybe True (root <) tMax
                          in checkMin && checkMax
