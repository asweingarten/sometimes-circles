{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Diagrams.Prelude
import Diagrams.Color.XKCD
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Arc
import Linear.V2

import Debug.Trace

import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Categorical
import Data.Random.Source.StdGen
import Data.Time.Clock.POSIX

import Data.Maybe
import Data.List.Split

import Control.Monad.State

data Brush a = Arc a
             | None a
  deriving (Functor, Show)

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let src = mkStdGen seed

  -- let lowers = [10::Double, 20 .. 50]
  -- let uppers = [60::Double, 70 .. 100]
  -- let bounds = (,) <$> lowers <*> uppers
  -- current fave is l=10 and u=60
  let l = 10
  let u = 60
  let sometimesCircles = foldr atop mempty
                         . flip evalState src
                         . replicateM 45
                         $ sometimesCircle l u []


  let diagram = sometimesCircles

  renderCairo "./out.png" (dims $ V2 400 400) $ diagram # bgFrame 1 (fromAlphaColour darkGreyBlue)

accum :: Brush Double -> Double -> Double
accum (Arc d) acc  = d + acc
accum (None d) acc = d + acc

sampleUniformly :: Double -> Double -> State StdGen Double
sampleUniformly l u = do
  sample <- runRVar (uniform l u) StdRandom
  return sample


sometimesCircle :: Double -> Double -> [Brush (Double, Double)] -> State StdGen (Diagram B)
sometimesCircle l u arcs = do
  let sweeps = fmap (fmap snd) arcs
  let degreesCovered = (foldr accum 0 sweeps)
  if (degreesCovered >= 360)
    then do
      src <- get
      scaleFactor <- sampleUniformly 0.1 10
      originOffset <- sampleUniformly 0.1 1
      color <- flip runRVar StdRandom $ weightedCategorical
                [ (((1::Double)/3), seafoam)
                , ((1/3), aquaBlue)
                , ((1/3), offWhite)
                ]

      return $ lcA color
             . translateY ((-2)*originOffset)
             . translateX originOffset
             . scale scaleFactor
             . foldr atop mempty
             . fmap toArc
             $ arcs
    else do
      let remainingDegrees = (360-degreesCovered)
      arcLen <- sampleUniformly l u
      let clippedArcLen = case (arcLen > remainingDegrees) of
                True -> remainingDegrees
                False -> arcLen
      let flag = case (length arcs) of
                   0 -> True
                   _ -> case (last arcs) of
                          (Arc d) -> False
                          (None d) -> True
      let newArc = case (flag) of
                      True -> Arc (degreesCovered, clippedArcLen)
                      False -> None (degreesCovered, clippedArcLen)
      let newArcs = arcs ++ [newArc]
      sometimesCircle l u newArcs

toArc :: Brush (Double, Double) -> Diagram B
toArc (None (d, s)) = mempty
toArc (Arc  (d, s)) = arc (angleDir $ d @@ deg) (s @@ deg)


--
-- what about dashed circles controlled by a sinusoidal function?
-- what could an interface for that look like?
-- you have phase, frequency, and magnitude
-- good purescript project
