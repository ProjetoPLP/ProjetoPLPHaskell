module Main where
import CadastrarCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)
