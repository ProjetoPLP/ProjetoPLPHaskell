module Main where
import CadastrarCompany
import SearchCompany
import GetAttrsCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)