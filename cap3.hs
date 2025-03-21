-- 3.1)
data Pergunta = Sim | Nao deriving (Show)

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> (Int, Int)
listPergs [] = (0, 0)
listPergs (Sim:xs) = let (s, n) = listPergs xs in (s + 1, n)
listPergs (Nao:xs) = let (s, n) = listPergs xs in (s, n + 1)

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' _ _ = Nao

or' :: Pergunta -> Pergunta -> Pergunta
or' Nao Nao = Nao
or' _ _ = Sim

not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

-- 3.2)
data Temperatura = Celsius | Farenheit | Kelvin deriving Show

converterCelsius :: Double -> Temperatura -> Double
converterCelsius valor Celsius = valor
converterCelsius valor Farenheit = (valor - 32) * 5 / 9
converterCelsius valor Kelvin = valor - 273.15

converterKelvin :: Double -> Temperatura -> Double
converterKelvin valor Celsius = valor + 273.15
converterKelvin valor Farenheit = (valor - 32) * 5 / 9 + 273.15
converterKelvin valor Kelvin = valor

converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit valor Celsius = valor * 9 / 5 + 32
converterFarenheit valor Farenheit = valor
converterFarenheit valor Kelvin = (valor - 273.15) * 9 / 5 + 32

-- 3.3)
data Jogada = Pedra | Papel | Tesoura deriving (Show, Eq)

vencedor :: Jogada -> Jogada -> String
vencedor Pedra Pedra = "Empate"
vencedor Papel Papel = "Empate"
vencedor Tesoura Tesoura = "Empate"
vencedor Pedra Tesoura = "Pedra vence Tesoura"
vencedor Tesoura Papel = "Tesoura vence Papel"
vencedor Papel Pedra = "Papel vence Pedra"
vencedor Tesoura Pedra = "Tesoura vence Pedra"
vencedor Papel Tesoura = "Papel vence Tesoura"
vencedor Pedra Papel = "Papel vence Pedra"

-- 3.4)
removerVogais :: String -> String
removerVogais str = [x | x <- str, not (elem x "aeiouAEIOU")]

-- 3.5)
data UnidadeImperial = Inch | Yard | Foot deriving (Show, Eq)

converterMetros :: Double -> UnidadeImperial -> Double
converterMetros valor Inch = valor * 0.0254
converterMetros valor Yard = valor * 0.9144
converterMetros valor Foot = valor * 0.3048

converterImperial :: Double -> UnidadeImperial -> Double
converterImperial metros Inch = metros / 0.0254
converterImperial metros Yard = metros / 0.9144
converterImperial metros Foot = metros / 0.3048

-- 3.6)
data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho 
         | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
         deriving (Show, Eq, Enum, Bounded)

checaFim :: Mes -> Int
checaFim Fevereiro = 28
checaFim Abril = 30
checaFim Junho = 30
checaFim Setembro = 30
checaFim Novembro = 30
checaFim _ = 31

prox :: Mes -> Mes
prox Dezembro = Janeiro
prox mes = succ mes

data Hemisferio = Norte | Sul deriving (Show, Eq)

estacao :: Mes -> Hemisferio -> String
estacao mes Norte
    | mes `elem` [Dezembro, Janeiro, Fevereiro] = "Inverno"
    | mes `elem` [Marco, Abril, Maio] = "Primavera"
    | mes `elem` [Junho, Julho, Agosto] = "Verão"
    | otherwise = "Outono"

estacao mes Sul
    | mes `elem` [Dezembro, Janeiro, Fevereiro] = "Verão"
    | mes `elem` [Marco, Abril, Maio] = "Outono"
    | mes `elem` [Junho, Julho, Agosto] = "Inverno"
    | otherwise = "Primavera"

-- 3.7)
ehPalindromo :: String -> Bool
ehPalindromo str = str == reverse str

-- 3.8)
filtrarLista :: [Int] -> [Int]
filtrarLista xs = reverse [x | x <- xs, odd x, x >= 0, x `mod` 7 /= 0]

-- 3.9)
reverterStrings :: String -> String -> String -> (String, String, String)
reverterStrings x y z = (reverse x, reverse y, reverse z)

-- 3.10)
revNum :: Int -> String -> String
revNum n s = reverse (take n s) ++ drop n s

-- 3.11)
data Binario = Zero | Um deriving (Show, Eq)

data Funcao = Soma2 | Maior | Menor | Mult2 deriving (Show, Eq)

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Zero Um   = Um
aplicar Soma2 Um Zero   = Um
aplicar Soma2 Um Um     = Zero

aplicar Maior Zero Zero = Zero
aplicar Maior Zero Um   = Um
aplicar Maior Um Zero   = Um
aplicar Maior Um Um     = Um

aplicar Menor Zero Zero = Zero
aplicar Menor Zero Um   = Zero
aplicar Menor Um Zero   = Zero
aplicar Menor Um Um     = Um

aplicar Mult2 Zero Zero = Zero
aplicar Mult2 Zero Um   = Zero
aplicar Mult2 Um Zero   = Zero
aplicar Mult2 Um Um     = Um

-- 3.12)
binList :: [Binario] -> [Int]
binList xs = [if b == Zero then 1 else 0 | b <- xs]

-- 3.13)
data Metros = Metros Int Double | MetragemInvalida deriving (Show, Eq)

areaQuadrado :: Metros -> Metros
areaQuadrado (Metros 1 lado) = Metros 2 (lado * lado)
areaQuadrado _ = MetragemInvalida

areaRet :: Metros -> Metros -> Metros
areaRet (Metros 1 largura) (Metros 1 altura) = Metros 2 (largura * altura)
areaRet _ _ = MetragemInvalida

areaCubo :: Metros -> Metros
areaCubo (Metros 1 aresta) = Metros 2 (6 * aresta * aresta)
areaCubo _ = MetragemInvalida

-- 3.14)
data Valido = Yes String | No deriving (Show)

isNomeValido :: String -> Valido
isNomeValido "" = No
isNomeValido nome = Yes nome

main :: IO ()
main = do
    putStrLn "Exercícios capítulo 3:\n"

    -- 3.1)
    putStrLn "3.1) "
    -- Teste da função pergNum
    let p1 = Sim
    let p2 = Nao
    putStrLn ("pergNum Sim: " ++ show (pergNum p1))  -- Esperado: 1
    putStrLn ("pergNum Nao: " ++ show (pergNum p2))  -- Esperado: 0

    -- Teste da função listPergs
    let perguntas = [Sim, Nao, Sim, Sim, Nao]
    putStrLn ("Quantidade de Sim: " ++ show (fst (listPergs perguntas)))  -- Esperado: 3
    putStrLn ("Quantidade de Nao: " ++ show (snd (listPergs perguntas)))  -- Esperado: 2

    -- Teste da função and'
    putStrLn ("and' Sim Sim: " ++ show (and' Sim Sim))  -- Esperado: Sim
    putStrLn ("and' Sim Nao: " ++ show (and' Sim Nao))  -- Esperado: Nao
    putStrLn ("and' Nao Nao: " ++ show (and' Nao Nao))  -- Esperado: Nao

    -- Teste da função or'
    putStrLn ("or' Sim Sim: " ++ show (or' Sim Sim))  -- Esperado: Sim
    putStrLn ("or' Sim Nao: " ++ show (or' Sim Nao))  -- Esperado: Sim
    putStrLn ("or' Nao Nao: " ++ show (or' Nao Nao))  -- Esperado: Nao

    -- Teste da função not'
    putStrLn ("not' Sim: " ++ show (not' Sim))  -- Esperado: Nao
    putStrLn ("not' Nao: " ++ show (not' Nao))  -- Esperado: Sim

    -- 3.2)
    putStrLn "\n3.2) "
    -- Testando as conversões
    let valorCelsius = 25.0
    let valorFarenheit = 77.0
    let valorKelvin = 298.15
    
    putStrLn $ "25.0 Celsius em Farenheit: " ++ show (converterFarenheit valorCelsius Celsius)
    putStrLn $ "77.0 Farenheit em Celsius: " ++ show (converterCelsius valorFarenheit Farenheit)
    putStrLn $ "298.15 Kelvin em Celsius: " ++ show (converterCelsius valorKelvin Kelvin)
    putStrLn $ "25.0 Celsius em Kelvin: " ++ show (converterKelvin valorCelsius Celsius)
    putStrLn $ "77.0 Farenheit em Kelvin: " ++ show (converterKelvin valorFarenheit Farenheit)
    putStrLn $ "298.15 Kelvin em Farenheit: " ++ show (converterFarenheit valorKelvin Kelvin)

    -- 3.3)
    putStrLn "\n3.3) "
    putStrLn $ "Resultado da partida: " ++ vencedor Pedra Tesoura
    putStrLn $ "Resultado da partida: " ++ vencedor Papel Papel

    -- 3.4)
    putStrLn "\n3.4) "
    putStrLn (removerVogais "Olá, Haskell!")

    -- 3.5)
    putStrLn "\n3.5) "
    putStrLn "Conversão de 10 inches para metros:"
    print (converterMetros 10 Inch)

    putStrLn "Conversão de 5 yards para metros:"
    print (converterMetros 5 Yard)

    putStrLn "Conversão de 3 metros para feet:"
    print (converterImperial 3 Foot)

    -- 3.6)
    putStrLn "\n3.6) "
    let mesAtual = Maio
    putStrLn $ "Dias de " ++ show mesAtual ++ ": " ++ show (checaFim mesAtual)
    putStrLn $ "Próximo mês depois de " ++ show mesAtual ++ ": " ++ show (prox mesAtual)
    putStrLn $ "Estação de " ++ show mesAtual ++ " no Hemisfério Sul: " ++ estacao mesAtual Sul
    putStrLn $ "Estação de " ++ show mesAtual ++ " no Hemisfério Norte: " ++ estacao mesAtual Norte

    -- 3.7)
    putStrLn "\n3.7) "
    putStrLn $ "ehPalindromo \"arara\": " ++ show (ehPalindromo "arara")
    putStrLn $ "ehPalindromo \"haskell\": " ++ show (ehPalindromo "haskell")
    putStrLn $ "ehPalindromo \"ana\": " ++ show (ehPalindromo "ana")

    -- 3.8)
    putStrLn "\n3.8) "
    let lista1 = [1, 2, 3, 7, 14, 21, 28, -5, -7, 35, 49, 50, 77]
    let lista2 = [-10, -3, 0, 5, 6, 14, 21, 22, 25, 42, 49, 51]

    putStrLn "Lista original e filtrada (invertida):"
    putStrLn $ "Entrada: " ++ show lista1
    putStrLn $ "Saída:   " ++ show (filtrarLista lista1)

    putStrLn "\nOutro exemplo:"
    putStrLn $ "Entrada: " ++ show lista2
    putStrLn $ "Saída:   " ++ show (filtrarLista lista2)

    -- 3.9)
    putStrLn "\n3.9) "
    let (a, b, c) = reverterStrings "Haskell" "Functional" "Programming"

    putStrLn "Entrada: (\"Haskell\", \"Functional\", \"Programming\")"
    putStrLn $ "Saída:   (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

    let (d, e, f) = reverterStrings "abc" "12345" "XYZ"

    putStrLn "\nOutro exemplo:"
    putStrLn $ "Entrada: (\"abc\", \"12345\", \"XYZ\")"
    putStrLn $ "Saída:   (" ++ show d ++ ", " ++ show e ++ ", " ++ show f ++ ")"

    -- 3.10)
    putStrLn "\n3.10) "
    putStrLn $ "revNum 4 \"FATEC\"  = " ++ show (revNum 4 "FATEC")
    putStrLn $ "revNum 3 \"Programacao\" = " ++ show (revNum 3 "Programacao")
    putStrLn $ "revNum 5 \"Haskell\" = " ++ show (revNum 5 "Haskell")
    putStrLn $ "revNum 2 \"ABCD\" = " ++ show (revNum 2 "ABCD")
    putStrLn $ "revNum 7 \"OpenAI\" = " ++ show (revNum 7 "OpenAI")

    -- 3.11)
    putStrLn "\n3.11) "
    putStrLn $ "aplicar Soma2 Um Um   = " ++ show (aplicar Soma2 Um Um)
    putStrLn $ "aplicar Soma2 Um Zero = " ++ show (aplicar Soma2 Um Zero)
    putStrLn $ "aplicar Maior Zero Um = " ++ show (aplicar Maior Zero Um)
    putStrLn $ "aplicar Menor Um Zero = " ++ show (aplicar Menor Um Zero)
    putStrLn $ "aplicar Mult2 Um Um   = " ++ show (aplicar Mult2 Um Um)

    -- 3.12)
    putStrLn "\n3.12) "
    let lista = [Um, Zero, Zero, Um, Zero]
    putStrLn $ "binList " ++ show lista ++ " = " ++ show (binList lista)

    -- 3.13)
    putStrLn "\n3.13) "
    putStrLn $ "Área do quadrado: " ++ show (areaQuadrado (Metros 1 2.0))
    putStrLn $ "Área do retângulo: " ++ show (areaRet (Metros 1 2.0) (Metros 1 3.0))
    putStrLn $ "Área do cubo: " ++ show (areaCubo (Metros 1 2.0))
    putStrLn $ "Tentativa inválida: " ++ show (areaQuadrado (Metros 4 5.0))

    -- 3.14)
    putStrLn "\n3.14) "
    print (isNomeValido "Haskell")
    print (isNomeValido "")

