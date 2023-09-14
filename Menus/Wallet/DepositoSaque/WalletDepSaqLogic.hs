module Menus.Wallet.DepositoSaque.WalletDepSaqLogic where
    
import Models.Client.GetSetAttrsClient ( addCash, setCash, removeCash, getCash, setCanDeposit )


depositar :: Int -> Bool -> IO ()
depositar idClient canDeposit = do
    if canDeposit then do
        addCash idClient 100
        setCanDeposit idClient False
    else putStrLn "Depósito negado. O cliente não realizou um saque anteriormente."


sacarTudo :: Int -> IO ()
sacarTudo idClient = do
    if getCash idClient >= 200 then do
        setCash idClient 0
        setCanDeposit idClient True
    else putStrLn "Saque negado. O cliente não possui um saldo de 200 reais ou mais."


sacar200 :: Int -> IO ()
sacar200 idClient = do
    if getCash idClient >= 200 then do
        removeCash idClient 200
        setCanDeposit idClient True
    else putStrLn "Saque negado. O cliente não possui um saldo de 200 reais ou mais."


sacar500 :: Int -> IO ()
sacar500 idClient = do
    if getCash idClient >= 500 then do
        removeCash idClient 500
        setCanDeposit idClient True
    else putStrLn "Saque negado. O cliente não possui um saldo de 200 reais ou mais."