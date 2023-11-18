module Utils.MatrixUtils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Matrix = [[Char]]


-- Printa no terminal o conteúdo do arquivo .txt
printMatrix :: FilePath -> IO ()
printMatrix filePath = do
    putStrLn "\ESC[H\ESC[2J"
    matrix <- readMatrixFromFile filePath
    mapM_ putStrLn matrix


-- Sobrescreve no arquivo .txt um novo valor
writeMatrixValue :: FilePath -> String -> Int -> Int -> IO ()
writeMatrixValue filePath val row col = do
    matrix <- readMatrixFromFile filePath
    let modifiedMatrix = foldr (\(d, c) m -> updateMatrix m row (col + c) d) matrix (zip val [0..])
    writeFile filePath (unlines modifiedMatrix)


-- Lê o arquivo .txt
readMatrixFromFile :: FilePath -> IO Matrix
readMatrixFromFile filePath = do
    contents <- TIO.readFile filePath
    return $ map T.unpack $ T.lines contents


-- Retorna uma nova Matrix modificada
updateMatrix :: Matrix -> Int -> Int -> Char -> Matrix
updateMatrix matrix row col newChar =
    let updateRow :: [Char] -> Int -> [Char]
        updateRow [] _ = []
        updateRow (x:xs) 0 = newChar : xs
        updateRow (x:xs) n = x : updateRow xs (n - 1)

        updateMatrix' :: Int -> Matrix -> Int -> Matrix
        updateMatrix' _ [] _ = []
        updateMatrix' 1 (row:rows) col = updateRow row (col - 1) : rows
        updateMatrix' n (row:rows) col = row : updateMatrix' (n - 1) rows col

    in updateMatrix' row matrix col