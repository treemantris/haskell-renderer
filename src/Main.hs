{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import Data.List
import Data.List.Split
import Data.Word
import qualified Codec.Picture.Types as M
import Wavefront
import System.Random

type Colour = (Word8, Word8, Word8)
type ColouredPoint = (Point, Colour)
type Point = (Int, Int)
type Vector = (Int, Int, Int)
type VectorD = (Double, Double, Double)
type Triangle = (Point, Point, Point)

main :: IO ()
main = do
  -- let triangleOld = drawTriangle' (10, 70) (50, 160) (70, 80)
  -- let triangleNew = drawTriangle'' (110, 70) (150, 160) (170, 80)
  -- createAndSaveImage width height [triangleNew]
  -- createAndSaveImage width height [triangleOld, triangleNew]
  lines <- getLinesFromFile width height
  createAndSaveImage width height lines
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

getLinesFromFile :: Int -> Int -> IO [[ColouredPoint]] -- move some of the lets into where?, use monad bind?
getLinesFromFile width height = do
  wavefrontFile <- readWavefrontFile "input.txt"
  let faces' = faces wavefrontFile
  let intensities = map getIntensity faces'
  let triangles = facesToTriangles width height faces'
  let zipped = zip triangles intensities
  let drawedTriangles = map (\(t, i) -> drawTriangle'' t i ) zipped
  -- let points' = facesToPoints faces'
  -- let scaledPoints' = map (\(x0, y0, x1, y1) -> (scalex x0, scaley y0, scalex x1, scaley y1)) points'
  -- let lines = map (\(x0, y0, x1, y1) -> invertedGetPoints x0 y0 x1 y1) scaledPoints'
  -- return lines
  return drawedTriangles
  where
    scalex x = round $ (fromIntegral width - 1) * (x + 1) * 0.5
    scaley y = round $ (fromIntegral height - 1) * (y + 1) * 0.5
    invert y = height - y - 1
    invertedGetPoints x0 y0 x1 y1 = getPoints x0 (invert y0) x1 (invert y1)
    points = invertedGetPoints 1 1 200 200

createAndSaveImage width height lines = (savePngImage "output.png" . ImageRGB8) (createImage width height lines)

facesToTriangles :: Int -> Int -> [Face] -> [(Point, Point, Point)]
facesToTriangles width height faces = map faceToTriangle faces
  where
    faceToTriangle (vertex1, vertex2, vertex3)  = (vertexToPoint vertex1, vertexToPoint vertex2, vertexToPoint vertex3)
    vertexToPoint vertex = (scaledX vertex, scaledY vertex)
    scaledX vertex = round $ (fromIntegral width - 1) * ((x vertex) + 1) * 0.5
    scaledY vertex = height - 1 - (round $ (fromIntegral height - 1) * ((y vertex) + 1) * 0.5) -- inverting as well

facesToPoints :: [Face] -> [(Double, Double, Double, Double)]
facesToPoints faces = concatMap faceToPoints faces
  where
    faceToPoints (vertex1, vertex2, vertex3) = [vertexToVertex vertex1 vertex2, vertexToVertex vertex2 vertex3, vertexToVertex vertex3 vertex1]
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
        

createImage :: Int -> Int -> [[ColouredPoint]] -> Image PixelRGB8
createImage width height lines = runST $ do
  mimg <- M.createMutableImage width height (PixelRGB8 0 0 0)
  mapM_ (drawLine mimg) lines
  M.unsafeFreezeImage mimg

drawLine mimg pixels = mapM_ (\((x, y), (r, g, b)) -> writePixel mimg x y (PixelRGB8 r g b)) pixels

crossProduct :: Vector -> Vector -> Vector
crossProduct (ai, aj, ak) (bi, bj, bk) =
  (aj * bk - ak * bj, ak * bi - ai * bk, ai * bj - aj * bi)

crossProductD :: VectorD -> VectorD -> VectorD
crossProductD (ai, aj, ak) (bi, bj, bk) =
  (aj * bk - ak * bj, ak * bi - ai * bk, ai * bj - aj * bi)

dotProduct :: VectorD -> VectorD -> Double
dotProduct (ai, aj, ak) (bi, bj, bk) =
  ai * bi + aj * bj + ak * bk

normalise :: VectorD -> VectorD
normalise (a, b, c) = (a / mag, b / mag, c / mag)
  where
    -- mag = fromIntegral (sqrt ( a ^ 2 + b ^ 2 + c ^ 2)) -- wtf?
    mag = sqrt ( a ^ 2 + b ^ 2 + c ^ 2)
  
vertexSubtract :: Vertex -> Vertex -> VectorD
vertexSubtract a b =
  ((x a) - (x b), (y a) - (y b), (z a)  - (z b))

getIntensity :: Face -> Word8
getIntensity (a, b, c) = 
  round (255 * (dotProduct lightDirection normal))
  where
    ab = vertexSubtract b a 
    ac = vertexSubtract c a 
    bigNormal = crossProductD ab ac
    normal = normalise bigNormal
    lightDirection = (0, 0, -1)

drawTriangle'' :: Triangle -> Word8 ->  [ColouredPoint]
drawTriangle'' (a@(x0, y0), b@(x1, y1),  c@(x2, y2)) intensity  =
  [((x, y), colour) | (x, y) <- (bbox a b c), inTriangle (x, y) (a, b, c)] 
  where
    -- f = mkStdGen $ x0 + y0 + x1 + y1 + x2 + y2
    -- [red, green, blue] = take 3 $ randoms f
    colour = (intensity , intensity , intensity )

bbox :: Point -> Point -> Point -> [Point]
bbox (ax, ay) (bx, by) (cx, cy) = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
  where 
    minX = min ax $ min bx cx
    maxX = max ax $ max bx cx
    minY = min ay $ min by cy
    maxY = max ay $ max by cy

inTriangle :: Point -> Triangle -> Bool
inTriangle p t =
  u >= 0 && v >= 0 && w >= 0
  where
    (u, v, w) = baryCentric p t

baryCentric :: Point -> Triangle -> VectorD
baryCentric (px, py) (a@(ax, ay), b@(bx, by), c@(cx, cy)) =
  (u, v, w)
  where
    (uk, vk, k) = crossProduct (cy - ay, by - ay, ay - py) (cx - ax, bx - ax, ax - px)
    (u, v, w) = (fromIntegral uk / fromIntegral k , fromIntegral vk / fromIntegral k , (1 - u - v))
