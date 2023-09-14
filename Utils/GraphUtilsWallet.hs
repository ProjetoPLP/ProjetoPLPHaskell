module Utils.GraphUtilsWallet where

import Client.GetSetAttrsClient as Cli (getCol, getRow, updateCol, updateRow)
import Utils.MatrixUtils (writeMatrixValue)
import Client.ModelClient ( Client(ident) )


attClientLineRow :: Int -> Float -> Float -> IO ()
attClientLineRow idUser oldPatrimony newPatrimony = do
    checkClientColumn idUser

    if newPatrimony > oldPatrimony then do
        Cli.updateRow idUser (-1)
        checkClientRowOverflow idUser

    else if newPatrimony < oldPatrimony then do
        Cli.updateRow idUser 1
        checkClientRowUnderflow idUser

    else Cli.updateRow idUser 0


checkClientColumn :: Int -> IO ()
checkClientColumn idUser
    | Cli.getCol idUser > 95 = do
        cleanWLGraph ("./Client/Wallet/wallet" ++ show idUser ++ ".txt") 11
        Cli.updateCol idUser (-46)
    | otherwise =
        Cli.updateCol idUser 0


checkClientRowOverflow :: Int -> IO ()
checkClientRowOverflow idUser
    | Cli.getRow idUser <= 10 = do
        cleanWLGraph ("./Client/Wallet/wallet" ++ show idUser ++ ".txt") 11
        Cli.updateRow idUser 10
    | otherwise =
        Cli.updateRow idUser 0


checkClientRowUnderflow :: Int -> IO ()
checkClientRowUnderflow idUser
    | Cli.getRow idUser > 20 = do
        cleanWLGraph ("./Client/Wallet/wallet" ++ show idUser ++ ".txt") 11
        Cli.updateRow idUser (-10)
    | otherwise =
        Cli.updateRow idUser 0


attAllClientColumn :: [Client] -> IO ()
attAllClientColumn [] = return ()
attAllClientColumn (x:xs) = do
    Cli.updateCol (ident x) 2
    attAllClientColumn xs


cleanWLGraph :: FilePath -> Int -> IO ()
cleanWLGraph filepath 20 = writeMatrixValue filepath (replicate 47 ' ') 20 50
cleanWLGraph filepath row = do
    writeMatrixValue filepath (replicate 47 ' ') row 50
    cleanWLGraph filepath (row + 1)


updateWalletGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWalletGraphCandle filePath row col = do
    writeMatrixValue filePath "❚" row col