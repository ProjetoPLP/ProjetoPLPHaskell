module Main where
import CadastrarCliente
import GetAttrsClient

main :: IO()
main = do
    result <- cadastrarCliente
    putStrLn (result)
    putStrLn (getNome 1)
    putStrLn (getCPF 1)
