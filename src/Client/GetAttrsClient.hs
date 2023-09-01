module Client.GetAttrsClient where
import Client.SaveClient
import Client.ModelClient

getSaldo :: Int -> Float
getSaldo id = (cash (getClientsByID id (getClientJSON "./Data/Clients.json")))

getCPF :: Int -> Int
getCPF id = (cpf (getClientsByID id (getClientJSON "./Data/Clients.json")))

getNome :: Int -> String
getNome id = (name (getClientsByID id (getClientJSON "./Data/Clients.json")))

formatarCPF :: String -> String
formatarCPF cpf =
    let digitos = filter (`elem` ['0'..'9']) cpf  -- Remove caracteres não numéricos
    in case digitos of
        [] -> ""  -- Retorna uma string vazia se não houver dígitos
        _  -> let (parte1, resto1) = splitAt 3 digitos
                  (parte2, parte3) = splitAt 3 resto1
                  (parte4, parte5) = splitAt 3 parte3
              in parte1 ++ "." ++ parte2 ++ "." ++ parte4 ++ "-" ++ parte5
