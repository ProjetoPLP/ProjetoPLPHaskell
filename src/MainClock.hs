import Clock.GetSetClock

main :: IO()
main = do
    setClock (-1)
    result <- readClock
    putStrLn (show result)