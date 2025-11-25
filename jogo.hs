import Data.Char (toUpper)
import System.IO
import System.Random

--  ghc jogo.hs -o jogo

data Jogada = Pedra | Papel | Tesoura | Fogo | Agua
  deriving (Show, Read, Eq, Enum, Bounded)

data Modo = Normal | Deus | Tonto
  deriving (Show, Eq)

data Resultado = Vitoria | Derrota | Empate
  deriving (Show, Eq)

determinarVencedor :: Jogada -> Jogada -> Resultado
determinarVencedor jogador computador
  | jogador == computador = Empate
  | ganha jogador computador = Vitoria
  | otherwise = Derrota
  where
    ganha Pedra Tesoura = True
    ganha Tesoura Papel = True
    ganha Papel Pedra = True
    ganha Fogo Pedra = True
    ganha Fogo Papel = True
    ganha Fogo Tesoura = True
    ganha Pedra Agua = True
    ganha Papel Agua = True
    ganha Tesoura Agua = True
    ganha Agua Fogo = True
    ganha _ _ = False

numeroParaJogada :: Int -> Jogada
numeroParaJogada 1 = Pedra
numeroParaJogada 2 = Papel
numeroParaJogada 3 = Tesoura
numeroParaJogada 4 = Fogo
numeroParaJogada 5 = Agua
numeroParaJogada _ = Pedra

gerarJogadaAleatoria :: IO Jogada
gerarJogadaAleatoria = do
  num <- randomRIO (1, 5)
  return $ numeroParaJogada num

-- modo deus
modoDeusEscolher :: Jogada -> Jogada
modoDeusEscolher Pedra = Fogo
modoDeusEscolher Papel = Tesoura
modoDeusEscolher Tesoura = Pedra
modoDeusEscolher Fogo = Agua
modoDeusEscolher Agua = Papel

-- modo tonto
modoTontoEscolher :: Jogada -> Jogada
modoTontoEscolher Pedra = Papel
modoTontoEscolher Papel = Fogo
modoTontoEscolher Tesoura = Pedra
modoTontoEscolher Fogo = Pedra
modoTontoEscolher Agua = Fogo

escolherJogadaComputador :: Modo -> Jogada -> IO Jogada
escolherJogadaComputador Normal _ = gerarJogadaAleatoria
escolherJogadaComputador Deus jogadaJogador = return $ modoDeusEscolher jogadaJogador
escolherJogadaComputador Tonto jogadaJogador = return $ modoTontoEscolher jogadaJogador

exibirMenu :: IO ()
exibirMenu = do
  putStrLn "\n========================================="
  putStrLn "  PEDRA-PAPEL-TESOURA-FOGO-AGUA"
  putStrLn "========================================="
  putStrLn "Escolha sua jogada:"
  putStrLn "  1 - Pedra"
  putStrLn "  2 - Papel"
  putStrLn "  3 - Tesoura"
  putStrLn "  4 - Fogo"
  putStrLn "  5 - Agua"
  putStrLn "  0 - Sair"
  putStr "\nSua escolha: "
  hFlush stdout

exibirMenuModo :: IO Modo
exibirMenuModo = do
  putStrLn "\n========================================="
  putStrLn "  ESCOLHA O MODO DE JOGO"
  putStrLn "========================================="
  putStrLn "  1 - Modo Normal (jogo joga aleatoriamente)"
  putStrLn "  2 - Modo Deus (jogo sempre ganha)"
  putStrLn "  3 - Modo Tonto (jogo sempre perde)"
  putStr "\nSua escolha: "
  hFlush stdout
  entrada <- getLine
  case entrada of
    "1" -> return Normal
    "2" -> return Deus
    "3" -> return Tonto
    _ -> do
      putStrLn "Opcao invalida! Escolhendo modo Normal."
      return Normal

exibirResultado :: Jogada -> Jogada -> Resultado -> IO ()
exibirResultado jogadaJogador jogadaComputador resultado = do
  putStrLn $ "\nVoce escolheu: " ++ show jogadaJogador
  putStrLn $ "Computador escolheu: " ++ show jogadaComputador
  putStrLn ""
  case resultado of
    Vitoria -> putStrLn "*** Voce VENCEU! ***"
    Derrota -> putStrLn "*** Voce PERDEU! ***"
    Empate -> putStrLn "*** EMPATE! ***"
  putStrLn ""

loopJogo :: Modo -> IO ()
loopJogo modo = do
  exibirMenu
  entrada <- getLine
  case entrada of
    "0" -> putStrLn "\nObrigado por jogar! Ate logo!\n"
    "1" -> jogar Pedra modo
    "2" -> jogar Papel modo
    "3" -> jogar Tesoura modo
    "4" -> jogar Fogo modo
    "5" -> jogar Agua modo
    _ -> do
      putStrLn "\nOpcao invalida! Tente novamente."
      loopJogo modo
  where
    jogar jogadaJogador modoAtual = do
      jogadaComputador <- escolherJogadaComputador modoAtual jogadaJogador
      let resultado = determinarVencedor jogadaJogador jogadaComputador
      exibirResultado jogadaJogador jogadaComputador resultado
      loopJogo modoAtual

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\n========================================"
  putStrLn "  BEM-VINDO AO JOGO PEDRA-PAPEL-TESOURA"
  putStrLn "         FOGO E AGUA!"
  putStrLn "========================================"
  modo <- exibirMenuModo
  putStrLn $ "\nModo selecionado: " ++ show modo
  putStrLn "Vamos comecar!\n"
  loopJogo modo
