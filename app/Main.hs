{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude
import Diagrams.Color.XKCD
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Arc
import Linear.V2

import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.StdGen
import Data.Time.Clock.POSIX

import Data.Maybe

import Control.Monad.State

data Brush a = Arc a
             | None a

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let src = mkStdGen seed

  let sometimesCircles = fmap (\(s, sc) -> scale s sc)
                         . zip [0.1, 0.2 .. 5]
                         . flip evalState src
                         $ replicateM 50 sometimesCircle
  let topLeft = drift sometimesCircles $ V2 1 (-1)
  let topRight = drift sometimesCircles $ V2 (-1) (-1)
  let botLeft = drift sometimesCircles $ V2 (1) (1)
  let botRight = drift sometimesCircles $ V2 (-1) (1)
  let diagram' = center $ (center (topLeft ||| topRight)) === (center (botLeft ||| botRight))
  let diagram = (drift sometimesCircles (V2 0 0) # scale 4) `atop` diagram'


  renderCairo "./out.png" (dims $ V2 400 400) $ diagram # bgFrame 1 (fromAlphaColour darkNavy)

drift :: [Diagram B] -> V2 Double -> Diagram B
drift ds (V2 dx dy)=
  position $ zip (fmap mkPoint [0, 0.2 .. 5]) ds
  where mkPoint x = p2 (x*dx,x*dy)

sometimesCircle :: State StdGen (Diagram B)
sometimesCircle = do
  src <- get
  let (brushStrokes, (src', _)) = flip runState (src, None 0.0)
                                  . mapM assignPoint
                                  $ [0, 0.5 .. 360]
  let d = foldr atop mempty
          . fmap (lcA neonBlue)
          . fmap (\(dir, sweep) -> arc (angleDir dir) sweep)
          . fmap (\(dir, sweep) -> (dir @@ deg, sweep @@deg))
          . catMaybes
          . flip evalState []
          . mapM arcs
          $ brushStrokes
  put src'
  return d


arcs :: Brush Double -> State [Double] (Maybe (Double, Double))
arcs (None _) = do
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
arcs (Arc d) = do
  workingArc <- get
  put $ workingArc ++ [d]
  return Nothing

assignPoint :: Double -> State (StdGen, (Brush Double)) (Brush Double)
assignPoint d = do
  (src, prev) <- get
  let p = case prev of
            (None _) -> 0.20::Double
            (Arc _)  -> 0.90
  let (coin, src') = flip runState src $ runRVar (boolBernoulli p) StdRandom
  let brush = case coin of
                True -> Arc d
                False -> None d
  put (src', brush)
  return brush


-- what about dashed circles controlled by a sinusoidal function?
-- what could an interface for that look like?
-- you have phase, frequency, and magnitude
-- good purescript project
