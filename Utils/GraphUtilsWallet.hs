module Utils.GraphUtilsWallet where

import Utils.MatrixUtils ( writeMatrixValue )
import Models.Client.GetSetAttrsClient ( getCol, getRow, addCol, addRow, setCol, setRow )
import Models.Client.ModelClient ( Client(ident) )


-- Atualiza em uma carteira, a partir do seu ID, a nova linha e coluna baseado no novo patrimônio
attClientLineRow :: Int -> Float -> Float -> IO ()
attClientLineRow idUser oldPatrimony newPatrimony = do
    checkClientColumn idUser
    if newPatrimony > oldPatrimony then do
        addRow idUser (-1)
        checkClientRowOverflow idUser
    else if newPatrimony < oldPatrimony then do
        addRow idUser 1
        checkClientRowUnderflow idUser
    else addRow idUser 0


-- Verifica se a coluna do gráfico chegou no limite
checkClientColumn :: Int -> IO ()
checkClientColumn idUser
    | getCol idUser > 95 = do
        cleanWLGraph ("./Models/Client/Wallets/wallet" ++ show idUser ++ ".txt") 11
        setCol idUser 51
    | otherwise =
        addCol idUser 0


-- Verifica se a linha do gráfico chegou no limite superior
checkClientRowOverflow :: Int -> IO ()
checkClientRowOverflow idUser
    | getRow idUser < 11 = do
        cleanWLGraph ("./Models/Client/Wallets/wallet" ++ show idUser ++ ".txt") 11
        setRow idUser 20
    | otherwise =
        addRow idUser 0


-- Verifica se a linha do gráfico chegou no limite inferior
checkClientRowUnderflow :: Int -> IO ()
checkClientRowUnderflow idUser
    | getRow idUser > 20 = do
        cleanWLGraph ("./Models/Client/Wallets/wallet" ++ show idUser ++ ".txt") 11
        setRow idUser 11
    | otherwise =
        addRow idUser 0


-- Atualiza a próxima coluna em todos os gráficos
attAllClientColumn :: [Client] -> IO ()
attAllClientColumn [] = return ()
attAllClientColumn (x:xs) = do
    addCol (ident x) 2
    attAllClientColumn xs


-- Reinicia o gráfico da carteira sobrescrevendo todos os espaços com caracteres vazios
cleanWLGraph :: FilePath -> Int -> IO ()
cleanWLGraph filepath 20 = writeMatrixValue filepath (replicate 47 ' ') 20 50
cleanWLGraph filepath row = do
    writeMatrixValue filepath (replicate 47 ' ') row 50
    cleanWLGraph filepath (row + 1)


updateWalletGraphCandle :: FilePath -> Int -> Int -> IO ()
updateWalletGraphCandle filePath row col = do
    writeMatrixValue filePath "❚" row col