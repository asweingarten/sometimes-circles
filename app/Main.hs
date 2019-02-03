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

  -- let lowers = [10::Double, 20 .. 50]
  -- let uppers = [60::Double, 70 .. 100]
  -- let bounds = (,) <$> lowers <*> uppers
  -- current fave is l=10 and u=60
  let l = 10
  let u = 60
  let sometimesCircles = drift (V2 1 (-1))
                         . scale'
                         . fmap (lcA neonBlue)
                         . flip evalState src
                         . replicateM 25
                         $ sometimesCircle l u []

  let diagram = sometimesCircles

  renderCairo "./out.png" (dims $ V2 400 400) $ diagram # bgFrame 1 (fromAlphaColour darkNavy)

scale' :: [Diagram B] -> [Diagram B]
scale' ds = fmap (\(s,d) -> d # scale s)
            $ zip [1 .. (fromIntegral $ (+1) $ length ds)] ds

drift :: V2 Double -> [Diagram B] -> Diagram B
drift (V2 dx dy) ds =
  position $ zip points ds
  where mkPoint x = p2 (x*dx,x*dy)
        points = fmap mkPoint
                 . fmap (*0.1)
                 $ [0 .. (fromIntegral $ length ds)]


accum :: Brush Double -> Double -> Double
accum (Arc d) acc  = d + acc
accum (None d) acc = d + acc

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
