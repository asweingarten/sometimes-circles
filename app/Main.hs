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

  let lowers = [10::Double, 20 .. 50]
  let uppers = [60::Double, 70 .. 100]
  let bounds = (,) <$> lowers <*> uppers
  let sometimesCircles = chunksOf 5
                         . fmap (lcA neonBlue)
                         . flip evalState src
                         . sequence
                         . fmap (\(l, u) -> sometimesCircle l u [])
                         $ bounds

  let diagram = vsep 1
                . fmap (hsep 1)
                $ sometimesCircles

  -- need a growBy
  -- compose with drift somehow
  -- maybe alternate colors
  -- new model for generating arcs. current one has too many decisions
  -- let diagram = (drift sometimesCircles (V2 0 0) # scale 4)
  -- let diagram = sometimesCircles # lcA neonBlue


  renderCairo "./out.png" (dims $ V2 400 400) $ diagram # bgFrame 1 (fromAlphaColour darkNavy)

drift :: [Diagram B] -> V2 Double -> Diagram B
drift ds (V2 dx dy)=
  position $ zip (fmap mkPoint [0, 0.2 .. 5]) ds
  where mkPoint x = p2 (x*dx,x*dy)


accum :: Brush Double -> Double -> Double
accum (Arc d) acc  = d + acc
accum (None d) acc = d + acc

-- have min and max sweep lengths for Arc and None
-- sample a length from those intervals
-- if it goes over 360 degrees, clip it
-- before, the beginning and end of the computation were driven by an fmap over a list.
sometimesCircle :: Double -> Double -> [Brush (Double, Double)] -> State StdGen (Diagram B)
sometimesCircle l u arcs = do
  let sweeps = fmap (fmap snd) arcs
  let degreesCovered = (foldr accum 0 sweeps)
  if (degreesCovered >= 360)
    then do
      return $ foldr atop mempty $ fmap toArc $ traceShowId arcs
    else do
      let remainingDegrees = (360-degreesCovered)
      arcLen <- arcLength' l u
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

arcLength' :: Double -> Double -> State StdGen Double
arcLength' l u = do
  src <- get
  let (length, src') = flip runState src
                       $ runRVar (uniform l u) StdRandom
  put src'
  return length
--
-- what about dashed circles controlled by a sinusoidal function?
-- what could an interface for that look like?
-- you have phase, frequency, and magnitude
-- good purescript project
