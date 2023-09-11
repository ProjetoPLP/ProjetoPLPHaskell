module Client.GetInfoForMakeLogin where
import System.IO (hFlush, stdout)

getEmail :: IO String
getEmail = do
    putStr "Digite o email: "
    hFlush stdout
    input <- getLine
    return input

getPassword :: IO String
getPassword = do
    putStr "Digite a senha: "
    hFlush stdout
    input <- getLine
    return (read input :: String)
