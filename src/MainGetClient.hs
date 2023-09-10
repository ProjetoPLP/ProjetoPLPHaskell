import Client.GetSetAttrsClient
import Client.SaveClient
import Client.ModelClient
import Client.PostClient

main :: IO()
main = do
    -- putStrLn (show (getName 1))
    -- putStrLn (show (getAge 1))
    -- putStrLn (show (getCPF 1))
    -- putStrLn (show (getEmail 1))
    -- putStrLn (show (getPassword 1))
    -- putStrLn (show (getCash 1))
    -- putStrLn (show (getPatrimony 1))
    -- putStrLn (show (getCanDeposit 1))
    -- putStrLn (show (getRow 1))
    -- putStrLn (show (getCol 1))
    -- putStrLn (show (getAllAssets 1))

    -- addAsset 1 2 2
    print (getAssetsInCompany (getAllAssets 1) 2)
    -- setName 1 "ok"
    -- setAge 1 2
    -- setCPF 1 "40404040404"
    -- setEmail 1 "ok"
    -- setPassword 1 2
    -- setCash 1 2
    -- setPatrimony 1 2
    -- setCanDeposit 1 True
    -- setRow 1 2
    -- setCol 1 2


    -- putStrLn (show (getName 1))
    -- putStrLn (show (getAge 1))
    -- putStrLn (show (getCPF 1))
    -- putStrLn (show (getEmail 1))
    -- putStrLn (show (getPassword 1))
    -- putStrLn (show (getCash 1))
    -- putStrLn (show (getPatrimony 1))
    -- putStrLn (show (getCanDeposit 1))
    -- putStrLn (show (getRow 1))
    -- putStrLn (show (getCol 1))
    -- putStrLn (show (getAllAssets 1))
