module Main where
import Client.CadastrarCliente
import Client.GetAttrsClient

main :: IO()
main = do
    result <- cadastrarCliente
    putStrLn (result)
    putStrLn (show (getSaldo 1))
