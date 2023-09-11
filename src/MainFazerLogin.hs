import Client.RealizarLogin
import Client.SaveClient
import Client.ModelClient
import Client.GetSetAttrsClient

main :: IO()
main = do
    -- result <- fazerLogin

    client <- getCurrentUserID

    -- print (ident client)

    print client
