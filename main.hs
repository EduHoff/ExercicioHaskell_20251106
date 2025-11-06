import System.Process (system)

removerElementos :: (Eq a) => [a] -> [a]
removerElementos [] = []
removerElementos [x] = [x]
removerElementos (h:t)
    | h == head t = removerElementos t
    | otherwise = h : removerElementos t

main :: IO()
main = do
    clear
    let array = ["a","a","b","c"]

    putStrLn ("Original: " ++ show array)
    putStrLn ("Após função: " ++ show (removerElementos array))


clear :: IO ()
clear = system "cls || clear" >> return ()