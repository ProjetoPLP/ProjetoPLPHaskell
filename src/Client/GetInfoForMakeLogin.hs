module GetInfoForMakeLogin where
import System.IO (hFlush, stdout)

getEmail :: IO String
getEmail = do
    putStr "Digite o email: "
    hFlush stdout
    input <- getLine
    return input

getPassword :: IO Int
getPassword = do
    putStr "Digite a senha: "
    hFlush stdout
    input <- getLine
    return (read input :: Int)

