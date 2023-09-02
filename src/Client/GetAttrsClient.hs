module GetAttrsClient where
import SaveClient
import ModelClient

getSaldo :: Int -> Float
getSaldo id = (cash (getClientsByID id (getClientJSON "../Data/Clients.json")))

getCPF :: Int -> String
getCPF id = formatCPF (cpf (getClientsByID id (getClientJSON "../Data/Clients.json")))

getNome :: Int -> String
getNome id = (name (getClientsByID id (getClientJSON "../Data/Clients.json")))

setSaldo :: Int -> Float -> IO()
setSaldo id saldoAdicional = do
    let client = getClientsByID id (getClientJSON "../Data/Clients.json")
    let newCash = (cash client) + saldoAdicional
    let newClient = client {cash = newCash}
    editClientJSON "../Data/Clients.json" newClient

formatCPF :: Int -> String
formatCPF cpf =
  let cpfStr = show cpf
  in if length cpfStr == 11
       then
         let part1 = take 3 cpfStr
             part2 = take 3 (drop 3 cpfStr)
             part3 = take 3 (drop 6 cpfStr)
             part4 = drop 9 cpfStr
         in part1 ++ "." ++ part2 ++ "." ++ part3 ++ "-" ++ part4
       else "CPF inválido"
