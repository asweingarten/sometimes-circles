{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Arc
import Linear.V2

import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.StdGen
import Data.Time.Clock.POSIX

import Data.List.Split
import Control.Applicative
import Data.Coerce
import Data.List
import Data.Maybe

import Control.Monad.State

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let src = mkStdGen seed

  -- let diagram = arc (angleDir $ 0 @@ deg) (90 @@ deg)
  -- let diagram = sometimesCircle (0.0, 0.0) 1

  let arcs' = fmap (\(dir, sweep) -> arc (angleDir dir) sweep)
                   . fmap (\(dir, sweep) -> (dir @@ deg, sweep @@deg))
                   . catMaybes
                   . flip evalState []
                   $ forM (brushStrokes src) nom
  let diagram = foldr atop mempty arcs'

  renderCairo "./out.png" (dims $ V2 300 300) $ diagram # bgFrame 1 white

data Brush a = Arc a
             | None a

nom :: Brush Double -> State [Double] (Maybe (Double, Double))
nom (None _) = do
  workingArc <- get
  put []
  if (length workingArc < 2)
     then do
       return Nothing
     else do
       let direction = head workingArc
       let end = last workingArc
       let sweep = end - direction
       return $ Just (direction, sweep)
nom (Arc d) = do
  workingArc <- get
  put $ workingArc ++ [d]
  return Nothing

-- State s <thing I need that is obtained using s>
-- one function will be the mega computation
-- it will call other functions I had

getArc :: [Brush Double] -> Maybe (Double, Double)
getArc [] = Nothing
getArc ((None d):pts) = Nothing
getArc ((Arc d):pts) =
  let (Arc final) = last pts
   in
    Just (d, (final - d))


foo :: State [(Double, Double)] ()
foo = undefined

brushStrokes :: StdGen -> [Brush Double]
brushStrokes src =
  fmap (\(d, c) ->  if c then (Arc d) else (None d))
  $ zip degrees coinFlips
    where
      degrees = [0, 0.5 .. 360]
      (coinFlips, src') = (flip runState) src $ replicateM (length degrees) coinFlip'

coinFlip :: StdGen -> (Bool, StdGen)
coinFlip src =
  runState (runRVar (boolBernoulli (0.5::Double)) StdRandom) src

coinFlip' :: State StdGen Bool
coinFlip' =
  runRVar (boolBernoulli (0.5::Double)) StdRandom

circlePoints :: (Double, Double) -> Double -> [P2 Double]
circlePoints (cx, cy) r =
  (flip fmap) [0, 0.5 .. 360] $ p2 . \d ->
                    let
                      rads = d * (pi / 180)
                      rx = r * cos (rads)
                      ry = r * sin (rads)
                      x = cx + rx
                      y = cy + ry
                    in
                      (x, y)

-- Get the points of a circle's perimeter
-- at every point, flip a koin to decide if we connect the next point into an arc
-- when in connecting point state, then it's biased to stay that way and vice versa
--  (monad?)
-- do this multiple times with circles whose origins drift along a path (start with diagonal)
