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

  let arcBounds          = (10,160)
  let scaleBounds        = (0.1, 10)
  let originOffsetBounds = (-0.1, 0.5)
  let (sometimesCircles, src')   = flip runState src
                                   . replicateM 55
                                   $ sometimesCircle arcBounds scaleBounds originOffsetBounds []
  let (polkaDots, src'') = flip runState src'
                           . replicateM 5
                           . fmap (opacity 0.9)
                           $ polkaDot ((-21), 21) (0.1, 0.5)

  let polkaDots' = foldr atop mempty polkaDots
  let art = polkaDots' `atop` center (foldr atop mempty sometimesCircles)

  -- Single sometimes-circle
  -- let sometimesCircles = flip evalState src
                         -- $ sometimesCircle arcBounds scaleBounds originOffsetBounds []

  let diagram = art `atop` (unitCircle # scale 11 # lw none)

  renderCairo "./out.png" (dims $ V2 400 400) $ diagram # bgFrame 1 (fromAlphaColour darkGreyBlue)

accum :: Brush Double -> Double -> Double
accum (Arc d) acc  = d + acc
accum (None d) acc = d + acc

sampleUniformly :: Double -> Double -> State StdGen Double
sampleUniformly l u = do
  sample <- runRVar (uniform l u) StdRandom
  return sample

sometimesCircle ::  (Double,Double) -> (Double,Double) -> (Double,Double) -> [Brush (Double, Double)] -> State StdGen (Diagram B)
sometimesCircle (arcL, arcU) (scaleL, scaleU) (originL, originU) arcs = do
  let sweeps = fmap (fmap snd) arcs
  let degreesCovered = (foldr accum 0 sweeps)
  if (degreesCovered >= 360)
    then do
      src <- get
      scaleFactor <- sampleUniformly scaleL scaleU -- 0.1 10
      originOffset <- sampleUniformly originL originU -- 0.1 1
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
      let startingAngle = degreesCovered
      let remainingDegrees = (360-startingAngle)
      arcLen <- sampleUniformly arcL arcU
      let clippedArcLen = case (arcLen > remainingDegrees) of
                            True -> remainingDegrees
                            False -> arcLen


      flip <- runRVar (boolBernoulli (0.5::Double)) StdRandom
      let newArc = case (length arcs) of
                     -- should really be a coin flip
                     0 -> case (flip) of
                            True -> Arc (startingAngle, clippedArcLen)
                            False -> None (startingAngle, clippedArcLen)
                     _ -> case (last arcs) of
                            (Arc _)  -> None (startingAngle, clippedArcLen)
                            (None _) -> Arc (startingAngle, clippedArcLen)
      let newArcs = arcs ++ [newArc]
      sometimesCircle (arcL, arcU) (scaleL, scaleU) (originL, originU) newArcs

toArc :: Brush (Double, Double) -> Diagram B
toArc (None (d, s)) = mempty
toArc (Arc  (d, s)) = arc (angleDir $ d @@ deg) (s @@ deg)

polkaDot :: (Double,Double) -> (Double,Double) -> State StdGen (Diagram B)
polkaDot (originL, originU) (scaleL, scaleU) = do
  driftX <- sampleUniformly originL originU
  driftY <- sampleUniformly originL originU

  scaleFactor  <- sampleUniformly scaleL scaleU

  return $ unitCircle
           # translateX driftX
           # translateY driftY
           # scale scaleFactor
           # fcA golden
           # lw none

--
-- what about dashed circles controlled by a sinusoidal function?
-- what could an interface for that look like?
-- you have phase, frequency, and magnitude
-- good purescript project
