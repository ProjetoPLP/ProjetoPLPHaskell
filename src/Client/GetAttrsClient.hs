module GetAttrsClient where
import SaveClient
import ModelClient

getSaldo :: Int -> Float
getSaldo id = (cash (getClientsByID id (getClientJSON "../Data/Clients.json")))
