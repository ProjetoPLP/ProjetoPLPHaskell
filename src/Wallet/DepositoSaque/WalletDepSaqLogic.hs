module Wallet.DepositoSaque.WalletDepSaqLogic where
import Client.GetSetAttrsClient (addCash, setCash, removeCash, getCash)


depositar :: Int -> Bool -> IO ()
depositar idClient canDeposit = do
    if canDeposit then addCash idClient 100
    else putStrLn "Depósito negado. O cliente não realizou um saque anteriormente."


sacar :: Int -> String -> IO ()
sacar idClient input = do
    let currentCash = getCash idClient
    if currentCash >= 500 then
        if input == "T" then sacarTudo idClient
        else if input == "5" then sacar500 idClient
        else if input == "1" && currentCash >= 1000 then sacar1000 idClient
        else return ()
    else putStrLn "Saque negado. O cliente não possui um saldo de 500 reais ou mais."


sacarTudo :: Int -> IO ()
sacarTudo idClient = do
    setCash idClient 0


sacar500 :: Int -> IO ()
sacar500 idClient = do
    removeCash idClient 500


sacar1000 :: Int -> IO ()
sacar1000 idClient = do
    removeCash idClient 1000