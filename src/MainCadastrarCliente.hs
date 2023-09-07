import Client.CadastrarCliente
import Client.GetAttrsClient

main :: IO()
main = do
    result <- cadastrarCliente
    putStrLn (show result)
    putStrLn (getNome 1)
    putStrLn (getCPF 1)
