import System.Process (system)

rE :: (Eq a) => [a] -> [a] --removerElementos
rE [] = []
rE [x] = [x]
rE (h:t) = 
    if vER h (h:t) 0
        then rE t --Se for repitido
        else h : rE t --Se não for repitido

vER :: (Eq a) => a -> [a] -> Int -> Bool --verificarElementosRepitidos
vER _ [] acc = acc > 1
vER x (h:t) acc =
    if x == h
        then vER x t (acc+1)
        else vER x t acc


main :: IO()
main = do
    clear
    let array = ["a","a","b","c","d","b","a"]

    putStrLn ("Original: " ++ show array)
    putStrLn ("Após função: " ++ show (rE array))


clear :: IO ()
clear = system "cls || clear" >> return ()