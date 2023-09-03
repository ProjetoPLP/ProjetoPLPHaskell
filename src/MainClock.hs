import Clock.GetSetClock

main :: IO()
main = do
    setClock (20)
    result <- readClock
    putStrLn (show result)