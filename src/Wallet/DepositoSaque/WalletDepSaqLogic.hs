module Wallet.DepositoSaque.WalletDepSaqLogic where
    
import Client.GetSetAttrsClient (addCash, setCash, removeCash, getCash, setCanDeposit)


depositar :: Int -> Bool -> IO ()
depositar idClient canDeposit = do
    if canDeposit then do
        addCash idClient 100
        setCanDeposit idClient False
    else putStrLn "Depósito negado. O cliente não realizou um saque anteriormente."


sacarTudo :: Int -> IO ()
sacarTudo idClient = do
    if getCash idClient >= 500 then do
        setCash idClient 0
        setCanDeposit idClient True
    else putStrLn "Saque negado. O cliente não possui um saldo de 500 reais ou mais."


sacar500 :: Int -> IO ()
sacar500 idClient = do
    if getCash idClient >= 500 then do
        removeCash idClient 500
        setCanDeposit idClient True
    else putStrLn "Saque negado. O cliente não possui um saldo de 500 reais ou mais."


sacar1000 :: Int -> IO ()
sacar1000 idClient = do
    if getCash idClient >= 1000 then do
        removeCash idClient 1000
        setCanDeposit idClient True
    else putStrLn "Saque negado. O cliente não possui um saldo de 500 reais ou mais."