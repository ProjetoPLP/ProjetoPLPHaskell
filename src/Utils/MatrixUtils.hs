{-# LANGUAGE OverloadedStrings #-}

module Utils.MatrixUtils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Matrix = [[Char]]

-- Lê o arquivo .txt
readMatrixFromFile :: FilePath -> IO Matrix
readMatrixFromFile filePath = do
    contents <- TIO.readFile filePath
    return $ map T.unpack $ T.lines contents


-- Atualiza uma linha row e coluna col com um caractere newChar
updateMatrix :: Matrix -> Int -> Int -> Char -> Matrix
updateMatrix matrix row col newChar =
    let (beforeRow, targetRow : afterRow) = splitAt (row - 1) matrix
        newRow = updateRowGraph targetRow (col - 1) newChar
    in beforeRow ++ newRow : afterRow


-- Atualiza uma linha do gráfico na coluna col, retorna a linha atualizada
updateRowGraph :: [Char] -> Int -> Char -> [Char]
updateRowGraph row col newChar =
    take col row ++ [newChar] ++ drop (col + 1) row


-- Sobrescreve no arquivo .txt a linha atualizada
writeValue :: FilePath -> String -> Int -> Int -> IO ()
writeValue filePath val row col = do
    matrix <- readMatrixFromFile filePath
    let modifiedMatrix = foldr (\(d, c) m -> updateMatrix m row (col + c) d) matrix (zip val [0..])
    writeFile filePath (unlines modifiedMatrix)


-- Printa no terminal o conteúdo do arquivo .txt
printMatrix :: FilePath -> IO ()
printMatrix filePath = do
    matrix <- readMatrixFromFile filePath
    mapM_ putStrLn matrix