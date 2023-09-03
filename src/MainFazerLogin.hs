import Client.RealizarLogin
import Client.SaveClient

main :: IO()
main = do
    result <- fazerLogin
    putStrLn result
