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

