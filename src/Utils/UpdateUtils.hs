module Utils.UpdateUtils where

-- Adiciona a uma string uma quantidade de caracteres vazios à esquerda,
-- baseado no limite fornecido e o tamanho da string.
fillLeft :: String -> Int -> String
fillLeft val limit = replicate spaces ' ' ++ val
    where spaces = max 0 (limit - length val)


-- Adiciona a uma string uma quantidade de caracteres vazios à direita
fillRight :: String -> Int -> String
fillRight val limit = val ++ replicate limit ' '


-- Reseta um menu para a versão original antes de ser modificado
resetMenu :: FilePath -> FilePath -> IO ()
resetMenu targetPath originalPath = do
    writeFile targetPath ""
    originalContent <- readFile originalPath
    appendFile targetPath originalContent