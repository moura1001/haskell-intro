lista21A :: [Int]
lista21A = take 7 (iterate (*11) 1)

lista21B :: [Int]
lista21B = take 30 (filter (\x -> x `mod` 4 /= 0) [1..])

lista21C :: [String]
lista21C = map (\c -> "A" ++ [c] ++ "BB") ['a'..'g']

lista21D :: [Int]
lista21D = take 10 (gerar 5 [3,3,6,3,6,3,3,6,3,3])
gerar :: Int -> [Int] -> [Int]
gerar _ [] = []
gerar n (x:xs) = n : gerar (n + x) xs

lista21E :: [Double]
lista21E = take 6 (iterate (/2) 1.0)

lista21F :: [Int]
lista21F = take 8 (iterate (+9) 1)

lista21G :: [Int]
lista21G = take 11 (scanl (+) 2 (cycle [2,4,2]))

lista21H :: [Char]
lista21H = scanl (\c n -> toEnum (fromEnum c + n)) '@' [1,2,1,1,2,3,2]

listaTamanhoPar :: String -> Bool
listaTamanhoPar s = even (length s)

listaVetorRevertido :: [String] -> [String]
listaVetorRevertido vetor = reverse vetor

listaTamanhosImpares :: [String] -> [Int]
listaTamanhosImpares vetor = map length (filter  (odd . length) vetor)

listaHeadComposto :: [a] -> a
listaHeadComposto = foldr1 (\x _ -> x) . take 1

listaPalindromo :: String -> Bool
listaPalindromo s = s == reverse s

listaMultiplicacoes :: Int -> (Int, Int, Int, Int)
listaMultiplicacoes x = (2*x, 3*x, 4*x, 5*x)

main :: IO ()
main = do
putStrLn "Exercícios capítulo 2:\n"

putStr("2.1 a) ")
print(lista21A)

putStr("2.1 b) ")
print(lista21B)

putStr("2.1 c) ")
print(lista21C)

putStr("2.1 d) ")
print(lista21D)

putStr("2.1 e) ")
print(lista21E)

putStr("2.1 f) ")
print(lista21F)

putStr("2.1 g) ")
print(lista21G)

putStr("2.1 h) ")
print(lista21H)

putStr("2.2) ")
print(listaTamanhoPar "Haskell")

putStr("2.3) ")
print(listaVetorRevertido ["Haskell", "é", "legal"])

putStr("2.4) ")
print(listaTamanhosImpares ["Haskell", "é", "legal", "na", "programação"])

putStr("2.5) ")
print(listaHeadComposto ["Haskell", "é", "legal", "na", "programação"])

putStr("2.6) ")
print(listaPalindromo "Haskell")

putStr("2.7) ")
print(listaMultiplicacoes 4)

