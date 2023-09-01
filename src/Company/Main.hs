module Client.Main where
import Client.CadastrarCompany
import Client.SearchCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)
