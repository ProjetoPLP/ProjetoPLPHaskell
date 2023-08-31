module Main where
import CadastrarCliente

main :: IO()
main = do
    result <- cadastrarCliente
    putStrLn (result)
