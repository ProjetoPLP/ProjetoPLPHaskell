module Main where
import CadastrarCompany
import SearchCompany

main :: IO()
main = do
    result <- cadastrarCompany
    putStrLn (result)
    putStrLn (show (searchAndFilterCompanyByName "Levi"))
    putStrLn (show (searchAndFilterCompanyByActing "Algo"))
    putStrLn (show (searchAndFilterCompanyByCode 12345))
    putStrLn (show (searchAndFilterBySome "Levi"))
