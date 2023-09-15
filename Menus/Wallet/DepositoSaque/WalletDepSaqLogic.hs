module Menus.Wallet.DepositoSaque.WalletDepSaqLogic where
    
import Models.Client.GetSetAttrsClient ( addCash, setCash, addCash, getCash, setCanDeposit )


depositar :: Int -> Bool -> IO ()
depositar idClient canDeposit
    | canDeposit = do
        addCash idClient 100
        setCanDeposit idClient False
    | otherwise = putStrLn "Depósito negado. O cliente não realizou um saque anteriormente."


sacarTudo :: Int -> IO ()
sacarTudo idClient
    | getCash idClient >= 200 = do
        setCash idClient 0
        setCanDeposit idClient True
    | otherwise = putStrLn "Saque negado. O cliente não possui um saldo de 200 reais ou mais."


sacar200 :: Int -> IO ()
sacar200 idClient
    | getCash idClient >= 200 = do
        addCash idClient (-200)
        setCanDeposit idClient True
    | otherwise = putStrLn "Saque negado. O cliente não possui um saldo de 200 reais ou mais."


sacar500 :: Int -> IO ()
sacar500 idClient
    | getCash idClient >= 500 = do
        addCash idClient (-500)
        setCanDeposit idClient True
    | otherwise = putStrLn "Saque negado. O cliente não possui um saldo de 200 reais ou mais."