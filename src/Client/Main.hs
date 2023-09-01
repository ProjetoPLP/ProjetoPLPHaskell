module Main where
import CadastrarCliente
import GetAttrsClient

main :: IO()
main = do
    result <- cadastrarCliente
    putStrLn (result)
    putStrLn (show (getSaldo 1))
