module Main where
import CadastrarCompany
import SearchCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)
