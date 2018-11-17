{-# LANGUAGE RankNTypes #-}

module Prim (Hit(..),
             Material,
             MaterialInteraction(..),
             Prim,
             dielectric,
             hit,
             lambertian,
             metal,
             scatterRay,
             sphere) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State.Strict (State, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import System.Random (RandomGen, random)

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Maybe (maybeToList)

import Colour (Colour, colour)
import Ray (Ray(..), pointAt, rayDirection)
import Vector (Vec3, dot, reverseVector, squaredLength, unit, vec, vectorLength)

data Hit = Hit { hitTime :: Float
               , hitLocation :: Vec3
               , hitNormal :: Vec3 -- unit vector
               , hitMaterial :: Material
               }

type HitFn = Ray -> Maybe Float -> Maybe Float -> Maybe Hit
newtype Prim = Prim [HitFn] deriving (Semigroup, Monoid)

hit :: Prim -> Ray -> Maybe Float -> Maybe Float -> Maybe Hit
hit (Prim prims) ray tMin tMax = 
    let testHit prim = prim ray tMin tMax
        possibleHits = fmap testHit prims -- todo: consider parallel evaluation
        hits = possibleHits >>= maybeToList
    in case hits of [] -> Nothing
                    _ -> Just (minimumBy (compare `on` hitTime) hits)

sphere :: Vec3 -> Float -> Material -> Prim
sphere centre radius material = Prim [hitSphere centre radius material]
    
hitSphere :: Vec3 -> Float -> Material -> HitFn
hitSphere centre radius material ray@(Ray origin direction) tMin tMax
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
                    in Hit t p ((p - centre) / vec radius) material
          inBounds root = let checkMin = maybe True (root >) tMin
                              checkMax = maybe True (root <) tMax
                          in checkMin && checkMax

data MaterialInteraction = MaterialInteraction { materialAttenuation :: Colour
                                               , materialScattered :: Ray
                                               }

-- this is pretty gnarly and means that a scene containing e.g. exclusively metal balls still requires
-- an (unused) rng - what's a better approach?
-- this is also the only bit requiring RankNTypes
newtype Material = Material (forall g. RandomGen g => Ray -> Hit -> State g (Maybe MaterialInteraction))

scatterRay :: RandomGen g => Material -> Ray -> Hit -> State g (Maybe MaterialInteraction)
scatterRay (Material f) r h = f r h

randomInUnitSphere :: RandomGen g => State g Vec3
randomInUnitSphere = do
  p <- state random
  let p' = (2 * p) - 1
  if squaredLength p' >= 1
  then randomInUnitSphere
  else pure p'

-- todo: i think the albedos should be colours

lambertian :: Colour -> Material
lambertian albedo = Material scatter
    where scatter _ Hit { hitLocation, hitNormal } = do
            r <- randomInUnitSphere
            let target = hitLocation + hitNormal + r
                scattered = Ray hitLocation (target - hitLocation)
                attenuation = albedo
            pure (Just (MaterialInteraction attenuation scattered))

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (vec (2 * (dot v n)) * n)

metal :: Colour -> Float -> Material
metal albedo fuzz = Material scatter
    where scatter ray Hit { hitLocation, hitNormal } = do
            r <- randomInUnitSphere
            let reflected = reflect (unit (rayDirection ray)) hitNormal
                reflected' = reflected + (vec fuzz * r)
                scattered = Ray hitLocation reflected'
                attenuation = albedo
                shouldReflect = dot (rayDirection scattered) hitNormal > 0
                result = MaterialInteraction attenuation scattered
            pure (if shouldReflect then Just result else Nothing)

refract :: Vec3 -> Vec3 -> Float -> Maybe Vec3 -- direction normal refractive-index
refract v n i = let uv = unit v
                    dt = dot uv n
                    discriminant = 1 - (i * i * (1 - (dt * dt)))
                    result = (vec i * (uv - (n * vec dt))) - (n * vec (sqrt discriminant))
                in if discriminant > 0 then Just result else Nothing

schlick :: Float -> Float -> Float
schlick cosine idx = let r0 = (1 - idx) / (1 + idx)
                         r1 = r0 * r0
                     in r1 + ((1 - r1) * ((1 - cosine) ** 5))

dielectric :: Float -> Material
dielectric i = Material scatter
    where scatter Ray { rayDirection } Hit { hitLocation, hitNormal } =
              let reflected = reflect rayDirection hitNormal
                  (outwardNormal, index, cosine) = normalIndex rayDirection hitNormal
                  attenuation = colour 1 1 1
                  reflectionProb = schlick cosine i
                  reflection = pure (MaterialInteraction attenuation (Ray hitLocation reflected))
                  refraction = do
                    refracted <- MaybeT (pure (refract rayDirection outwardNormal index))
                    r <- lift (state random)
                    guard (r >= reflectionProb)
                    pure (MaterialInteraction attenuation (Ray hitLocation refracted))
              in runMaybeT (refraction <|> reflection)

          normalIndex direction normal = let dot' = dot direction normal
                                             a = i * dot' / vectorLength direction
                                             b = negate (dot' / vectorLength direction)
                                         in if dot' > 0
                                         then (reverseVector normal, i, a)
                                         else (normal, recip i, b)
