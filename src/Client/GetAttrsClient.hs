module GetAttrsClient where
import SaveClient
import ModelClient

getSaldo :: Int -> Float
getSaldo id = (cash (getClientsByID id (getClientJSON "../Data/Clients.json")))

getCPF :: Int -> Int
getCPF id = (cpf (getClientsByID id (getClientJSON "../Data/Clients.json")))

getNome :: Int -> String
getNome id = (name (getClientsByID id (getClientJSON "../Data/Clients.json")))

setSaldo :: Int -> Float -> IO()
setSaldo id saldoAdicional = do
    let client = getClientsByID id (getClientJSON "../Data/Clients.json")
    let newCash = (cash client) + saldoAdicional
    let newClient = client {cash = newCash}
    editClientJSON "../Data/Clients.json" newClient
