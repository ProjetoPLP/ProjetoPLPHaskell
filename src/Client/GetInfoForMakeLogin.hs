module GetInfoForMakeLogin where

getEmail :: IO String
getEmail = do
    putStr "Digite o email"
    input <- getLine
    return input

getPassword :: IO Int
getPassword = do
    putStr "Digite a senha"
    input <- getLine
    return (read input :: Int)
