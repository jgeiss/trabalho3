{--
        Esqueleto de programa para geração de bubble cloud em Haskell.
        Mais informações em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


module Main where

import Text.Printf -- Oba, Haskell tem printf! :-)
import System.Random
import System.IO.Unsafe -- usar tipos IO Int

type Point     = (Float,Float)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 360

imageHeight :: Int
imageHeight = 360


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
            tags = (map fst pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs) --AQUI USAR TAGS EM OUTRA FUNCAO
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = 
        let raios = geraRaio dataset -- gera lista com raios (float)
        in map svgCircle (insereRaios raios) 
             

-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> String
svgCircle ((x,y),r) = 
            let colorR = unsafePerformIO geraRandomico 
                colorG = unsafePerformIO geraRandomico
                colorB = unsafePerformIO geraRandomico
            in printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r colorR colorG colorB
-- fonte: http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-IO-Unsafe.html#v%3AunsafePerformIO

-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h

geraPoints :: Float -> Float -> [Float] -> Float -> [Point]
geraPoints _ _ [] _ = []
geraPoints w h (x:xs) n = 
            let a = ((x + head xs) * 0.005)
                t = n + 0.01 
                cX = w + (a * t * (cos t))  
                cY = h + (a * t * (sin t))   
            in  (cX,cY) : (geraPoints head xs)          
-------------------------

-- funcao que ordena uma lista decrescente
ordena :: [Int] -> [Int]
ordena [] = []
ordena (a:x) = aux a (ordena x)

-- funcao auxiliar para ordenação
aux :: Int -> [Int] -> [Int]
aux a [] = [a]
aux a (b:x)
    | a >= b = a : (b:x)
    | otherwise = b : aux a x

geraRaio :: [Int] -> [Float]
geraRaio [] = [] 
geraRaio (x:xs) = ((fromIntegral x) / 100) : geraRaio xs

-- Gera um valor randomico de 0 a 255
geraRandomico :: IO Int
geraRandomico = getStdRandom (randomR (0,255))

-- recebe uma lista com raios e retorna uma lista de circulos
insereRaios :: [Float] -> [Circle] 
insereRaios [] = []
insereRaios (x:xs) = ((fromIntegral imageWidth/2, fromIntegral imageHeight/2), x) : insereRaios xs


