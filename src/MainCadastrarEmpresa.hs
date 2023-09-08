import Company.CadastrarCompany

main :: IO()
main = do
    result <- cadastrarCompany

    putStrLn (show result)
