module Utils where

import System.Random (randomRIO)
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C
import qualified Graphics.DrawingCombinators as Draw
import Data.IORef (readIORef)
import Control.Monad (when)
import Graphics.UI.GLFW (getTime)
import Data.Monoid

type V2 = Draw.R2
type D = Draw.R

rand :: (Draw.R, Draw.R) -> IO Draw.R
rand (a, b) = fmap realToFrac (randomRIO ((realToFrac a, realToFrac b) :: (Double, Double)))

randi :: (Int, Int) -> IO Int
randi = randomRIO

range :: (Draw.R, Draw.R) -> Draw.R -> Draw.R
range (a, b) r = (b - a) * r + a

tintA :: Draw.R -> C.Colour Draw.R -> Draw.Image Draw.Any -> Draw.Image Draw.Any
tintA a c = let C.RGB r g b = C.toSRGB c in Draw.tint (Draw.Color r g b a)

tint :: C.Colour Draw.R -> Draw.Image Draw.Any -> Draw.Image Draw.Any
tint c = tintA 1 c

norm :: (Draw.R, Draw.R) -> Draw.R
norm (x, y) = sqrt (x ** 2.0 + y ** 2.0)

whenRef r a = do
  b <- readIORef r
  when b a
        
getRTime :: IO Draw.R
getRTime = fmap realToFrac getTime

wrap :: Draw.R -> Draw.R
wrap x
  | x < -1 = wrap (x + 2)
  | x > 1 = wrap (x - 2)
  | otherwise = x

wrapRad :: Draw.R -> Draw.R
wrapRad phi
  | phi < 0 = phi + pi2
  | phi > pi2 = phi - pi2
  | otherwise = phi
    where pi2 = 2 * pi

wrapV :: (Draw.R, Draw.R) -> (Draw.R, Draw.R)
wrapV (x, y) = (wrap x, wrap y)

mconcatmap :: [a] -> (a -> Draw.Image Draw.Any) -> Draw.Image Draw.Any
mconcatmap lst f = mconcat (map f lst)
