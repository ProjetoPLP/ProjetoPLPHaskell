import Company.GetSetAttrsCompany

main :: IO()
main = do
    putStrLn (show(getName 1))
    putStrLn (show(getAge 1))
    putStrLn (show(getCNPJ 1))
    putStrLn (show(getActuation 1))
    putStrLn (show(getDeclaration 1))
    putStrLn (show(getCode 1))
    putStrLn (show(getPrice 1))
    putStrLn (show(getTrendIndicator 1))
    putStrLn (show(getMinPrice 1))
    putStrLn (show(getMaxPrice 1))
    putStrLn (show(getStartPrice 1))
    putStrLn (show(getRow 1))
    putStrLn (show(getCol 1))

    setName 1 "Empresa2"
    setAge 1 2024
    setCNPJ 1 10101010101010
    setActuation 1 "Algo"
    setDeclaration 1 "Declaro"
    setCode 1 "codigo"
    setPrice 1 100
    setTrendIndicator 1 '.'
    setMinPrice 1 100.00
    setMaxPrice 1 100.00
    setStartPrice 1 100.00
    setRow 1 2
    setCol 1 2
    
    putStrLn (show(getName 1))
    putStrLn (show(getAge 1))
    putStrLn (show(getCNPJ 1))
    putStrLn (show(getActuation 1))
    putStrLn (show(getDeclaration 1))
    putStrLn (show(getCode 1))
    putStrLn (show(getPrice 1))
    putStrLn (show(getTrendIndicator 1))
    putStrLn (show(getMinPrice 1))
    putStrLn (show(getMaxPrice 1))
    putStrLn (show(getStartPrice 1))
    putStrLn (show(getRow 1))
    putStrLn (show(getCol 1))