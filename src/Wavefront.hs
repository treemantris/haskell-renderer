module Wavefront(x, y, z, vertex1, vertex2, vertex3, vertices, faces, Vertex, Face, WavefrontFile, readWavefrontFile) where

data Vertex = Vertex { x :: Double, y :: Double, z :: Double} deriving Show
data Face = Face { vertex1 :: Vertex, vertex2 :: Vertex, vertex3 :: Vertex } deriving Show
data WavefrontFile = WavefrontFile { vertices :: [Vertex], faces :: [Face] } deriving Show

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
    newVertex = parseVertex rest
    newFace = parseFace rest vertices

parseVertex :: String -> Vertex
parseVertex vertexString = Vertex v1 v2 v3
  where [v1, v2, v3] = map read $ words vertexString

parseFace :: String -> [Vertex] -> Face
parseFace faceString vertices = Face vertex1 vertex2 vertex3
  where
    (index1, index2, index3) = parseFaceIndexes faceString
    (vertex1, vertex2, vertex3) = (vertices !! index1, vertices !! index2, vertices !! index3)

parseFaceIndexes :: String -> (Int, Int, Int)
parseFaceIndexes faceRow = (index1 - 1, index2 - 1, index3 -1)
  where 
    [index1, index2, index3] = map extractVertexIndex (words faceRow) 
    extractVertexIndex element = read $ takeWhile (/= '/') element
