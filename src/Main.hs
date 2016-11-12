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

data Vertex = Vertex { x :: Double, y :: Double, z :: Double} deriving Show
data Face = Face { vertex1 :: Vertex, vertex2 :: Vertex, vertex3 :: Vertex } deriving Show
data WavefrontFile = WavefrontFile { vertices :: [Vertex], faces :: [Face] } deriving Show

main :: IO ()
main = do
  wavefrontFile <- readWavefrontFile "input.txt"
  let faces' = faces wavefrontFile
  let points' = facesToPoints faces'
  let scaledPoints' = map (\(x0, y0, x1, y1) -> (scalex x0, scaley y0, scalex x1, scaley y1)) points'
  let lines = map (\(x0, y0, x1, y1) -> invertedGetPoints x0 y0 x1 y1) scaledPoints'
  createAndSaveImage' width height lines
  where 
    (width, height) = (1000, 1000)
    scalex x = round $ (fromIntegral width - 1) * (x + 1) * 0.5
    scaley y = round $ (fromIntegral height - 1) * (y + 1) * 0.5
    invert y = height - y - 1
    invertedGetPoints x0 y0 x1 y1 = getPoints x0 (invert y0) x1 (invert y1)
    points = invertedGetPoints 1 1 200 200

createAndSaveImage' width height lines = (savePngImage "output.png" . ImageRGB8) (createImage width height lines)

facesToPoints :: [Face] -> [(Double, Double, Double, Double)]
facesToPoints faces = concatMap faceToPoints faces
  where
    faceToPoints face = [vertexToVertex (vertex1 face) (vertex2 face), vertexToVertex (vertex2 face) (vertex3 face), vertexToVertex (vertex3 face) (vertex1 face)]
    vertexToVertex v1 v2 = (x v1, y v1, x v2, y v2)

readWavefrontFile :: String -> IO WavefrontFile
readWavefrontFile filePath = do
  fileContents <- readFile filePath
  let rows = lines fileContents
  let wavefrontFile = parseWavefrontFile rows
  return wavefrontFile

parseWavefrontFile :: [String] -> WavefrontFile
parseWavefrontFile rows = parseWavefrontFile' rows [] []

parseWavefrontFile' :: [String] -> [Vertex] -> [Face] -> WavefrontFile
parseWavefrontFile' [] vertices faces = WavefrontFile vertices faces
parseWavefrontFile' (row: rows) vertices faces
  | firstChars == "f " = parseWavefrontFile' rows vertices (newFace:faces)
  | firstChars == "v " = parseWavefrontFile' rows (vertices ++ [newVertex]) faces
  | otherwise = parseWavefrontFile' rows vertices faces
  where
    firstChars = take 2 row
    rest = drop 2 row
    (index1, index2, index3) = parseFaceIndexes rest
    (vertex1, vertex2, vertex3) = (vertices !! index1, vertices !! index2, vertices !! index3)
    newFace = Face vertex1 vertex2 vertex3
    [v1, v2, v3] = map read $ words rest
    newVertex = Vertex v1 v2 v3

parseFaceIndexes :: String -> (Int, Int, Int)
parseFaceIndexes faceRow = (index1 - 1, index2 - 1, index3 -1)
  where 
    [index1, index2, index3] = map extractVertexIndex (words faceRow) 
    extractVertexIndex element = read $ takeWhile (/= '/') element

generateImg :: (Int -> Int -> PixelRGB8) -> DynamicImage
generateImg func = ImageRGB8 (generateImage func 100 100)

originalFunc :: Int -> Int -> PixelRGB8
originalFunc 60 75 = PixelRGB8 0 0 0
originalFunc _ _ = PixelRGB8 255 255 255

getPoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
getPoints x0 y0 x1 y1 
  | x0 > x1 = getPoints x1 y1 x0 y0
  | otherwise = [if steep then (y, x) else (x, y) | 
      x <- [beginx..endx],
      let t = (fromIntegral (x - beginx)) / (fromIntegral (endx - beginx)),
      let y = round $ fromIntegral beginy * (1-t) + fromIntegral endy * t]
  where steep = abs (y1 - y0) > (x1 - x0)
        (beginx, endx, beginy, endy) = swap $ if steep then (y0, y1, x0, x1) else (x0, x1, y0, y1)
        swap tuple@(x, x', y, y') = if x > x' then (x', x, y', y) else tuple
        

createImage :: Int -> Int -> [[(Int, Int)]] -> Image PixelRGB8
createImage width height lines = runST $ do
  mimg <- M.createMutableImage width height (PixelRGB8 0 0 0)
  mapM (drawLine mimg) lines
  M.unsafeFreezeImage mimg

drawLine mimg points = mapM_ (\(x, y) -> writePixel mimg x y (PixelRGB8 255 255 255)) points
