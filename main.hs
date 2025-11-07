import System.Process (system)

removerElementos :: (Eq a) => [a] -> [a]
removerElementos [] = []
removerElementos [x] = [x]
removerElementos (h:t)
    | h == head t = removerElementos t
    | otherwise = h : removerElementos t

vER :: (Eq a) => a -> [a] -> Int -> Bool --verificarElementosRepitidos
vER _ [] acc = acc > 1
vER x (h:t) acc =
    if x == h
        then vER x t (acc+1)
        else vER x t acc


main :: IO()
main = do
    clear
    let array = ["a","a","b","c","d","b"]

    putStrLn ("Original: " ++ show array)
    putStrLn ("Após função: " ++ show (removerElementos array))
    putStrLn ("Teste função verificação: " ++ show (vER "b" array 0))


clear :: IO ()
clear = system "cls || clear" >> return ()