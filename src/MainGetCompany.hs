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
    putStrLn (show(getRow 1))
    putStrLn (show(getCol 1))

    setName 1 "2"
    setAge 1 2
    setCNPJ 1 22222222222222
    setActuation 1 "2"
    setDeclaration 1 "2"
    setCode 1 "2"
    setPrice 1 2
    setRow 1 2
    setCol 1 2
    
    putStrLn (show(getName 1))
    putStrLn (show(getAge 1))
    putStrLn (show(getCNPJ 1))
    putStrLn (show(getActuation 1))
    putStrLn (show(getDeclaration 1))
    putStrLn (show(getCode 1))
    putStrLn (show(getPrice 1))
    putStrLn (show(getRow 1))
    putStrLn (show(getCol 1))