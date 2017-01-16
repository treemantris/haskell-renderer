{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Data.List
import qualified Codec.Picture.Types as M
import Wavefront

type Point = (Int, Int)

main :: IO ()
main = do
  let triangle = drawTriangle' (10, 70) (50, 160) (70, 80)
  -- lines <- getLinesFromFile width height
  createAndSaveImage width height [triangle]
  where
    (width, height) = (1000, 1000)

drawTriangle :: Point -> Point -> Point -> [Point]
drawTriangle a b c = line1 ++ line2 ++ line3 ++ fillingLines
  where
    line1 = getPoints' a b
    line2 = getPoints' b c
    line3 = getPoints' c a
    (shortLine, shortLength, longLine, longLength) = if (length1 < length2) then (line1, length1, line2, length2) else (line2, length2, line1, length1)
    length1 = lengthOfLine a b
    length2 = lengthOfLine b c
    -- fillingLines = foldl1 (++) [getPoints' l s | l <- longLine, let t = fromIntegral l / longLength, let s = round $ t * shortLength ]
    fillingLines = foldl1 (++) [getPoints' l s | index <- [0..(length longLine - 1)], let l = longLine !! index, let t = fromIntegral index / longLength, let index2 = round $ t * (fromIntegral . length) shortLine, let s = shortLine !! index2]

drawTriangle' :: Point -> Point -> Point -> [Point]
drawTriangle' a@(x0, y0) b@(x1, y1) c@(x2, y2) 
  | y0 > y1 = drawTriangle' b a c
  | y2 < y0 = drawTriangle' c a b
  | y2 < y1 = drawTriangle' a c b
  | otherwise = firstSegment ++ lastSegment
  where
    firstSegment = foldl1 (++) [ getPoints startX y endX y | y <- [y0..y1], let t = (fromIntegral $ y - y0) / segmentHeight, let startX = x0 + round (t * (fromIntegral $ x1 - x0)), let endX = x0 + round ((fromIntegral $ y - y0) / totalHeight * (fromIntegral $ x2 - x0)) ] 
    lastSegment = foldl1 (++) [ getPoints startX y endX y | y <- [y1..y2], let t = (fromIntegral $ y - y1) / (fromIntegral $ y2 - y1), let startX = x1 + round (t * (fromIntegral $ x2 - x1)), let endX = x0 + round ((((fromIntegral $ y - y0) / (fromIntegral $ y2 - y0))) * (fromIntegral $ x2 - x0))]
    segmentHeight = fromIntegral $ y1 - y0
    totalHeight = fromIntegral $ y2 - y0

lengthOfLine :: Point -> Point -> Double
lengthOfLine (x0, y0) (x1, y1) = sqrt $ fromIntegral (x1 - x0) ^ 2 + fromIntegral (y1 - y0) ^ 2

getPoints' :: Point -> Point -> [Point]
getPoints' (x0, y0) (x1, y1) = getPoints x0 y0 x1 y1

getLinesFromFile :: Int -> Int -> IO [[Point]]
getLinesFromFile width height = do
  wavefrontFile <- readWavefrontFile "input.txt"
  let faces' = faces wavefrontFile
  let points' = facesToPoints faces'
  let scaledPoints' = map (\(x0, y0, x1, y1) -> (scalex x0, scaley y0, scalex x1, scaley y1)) points'
  let lines = map (\(x0, y0, x1, y1) -> invertedGetPoints x0 y0 x1 y1) scaledPoints'
  return lines
  where
    scalex x = round $ (fromIntegral width - 1) * (x + 1) * 0.5
    scaley y = round $ (fromIntegral height - 1) * (y + 1) * 0.5
    invert y = height - y - 1
    invertedGetPoints x0 y0 x1 y1 = getPoints x0 (invert y0) x1 (invert y1)
    points = invertedGetPoints 1 1 200 200

createAndSaveImage width height lines = (savePngImage "output.png" . ImageRGB8) (createImage width height lines)

facesToPoints :: [Face] -> [(Double, Double, Double, Double)]
facesToPoints faces = concatMap faceToPoints faces
  where
    faceToPoints face = [vertexToVertex (vertex1 face) (vertex2 face), vertexToVertex (vertex2 face) (vertex3 face), vertexToVertex (vertex3 face) (vertex1 face)]
    vertexToVertex v1 v2 = (x v1, y v1, x v2, y v2)

generateImg :: (Int -> Int -> PixelRGB8) -> DynamicImage
generateImg func = ImageRGB8 (generateImage func 100 100)

originalFunc :: Int -> Int -> PixelRGB8
originalFunc 60 75 = PixelRGB8 0 0 0
originalFunc _ _ = PixelRGB8 255 255 255

getPoints :: Int -> Int -> Int -> Int -> [Point]
getPoints x0 y0 x1 y1 
  | x0 == x1 && y0 == y1 = [(x0, y0)]
  | x0 > x1 = getPoints x1 y1 x0 y0
  | otherwise = [if steep then (y, x) else (x, y) | 
      x <- [beginx..endx],
      let t = (fromIntegral (x - beginx)) / (fromIntegral (endx - beginx)),
      let y = round $ fromIntegral beginy * (1-t) + fromIntegral endy * t]
  where steep = abs (y1 - y0) > (x1 - x0)
        (beginx, endx, beginy, endy) = swap $ if steep then (y0, y1, x0, x1) else (x0, x1, y0, y1)
        swap tuple@(x, x', y, y') = if x > x' then (x', x, y', y) else tuple
        

createImage :: Int -> Int -> [[Point]] -> Image PixelRGB8
createImage width height lines = runST $ do
  mimg <- M.createMutableImage width height (PixelRGB8 0 0 0)
  mapM (drawLine mimg) lines
  M.unsafeFreezeImage mimg

drawLine mimg points = mapM_ (\(x, y) -> writePixel mimg x y (PixelRGB8 255 255 255)) points
