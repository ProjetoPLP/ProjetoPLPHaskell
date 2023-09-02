module Main where
import CadastrarCompany
import SearchCompany
import GetSetAttrsCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)
