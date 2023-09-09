import Client.RealizarLogin
import Client.SaveClient

import Client.ModelClient

main :: IO()
main = do
    -- result <- fazerLogin

    client <- getClientLogado

    -- print (ident client)

    print client
