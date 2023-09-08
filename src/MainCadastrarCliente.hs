import Client.CadastrarCliente

main :: IO()
main = do
    result <- cadastrarCliente
    if result then do
        putStrLn "ok"
    else putStrLn "nn"
