import Text.Printf
import System.Random
import Data.Maybe
import Data.List
import Data.Random.Extras
import System.IO.Unsafe  -- be careful!   


type Cell  = ((Int,Int),(Int,Int,Int,Int))
type Color = (Int,Int,Int)
type Coord = (Int,Int)


-------------------------------------------------------------------------------
-- Constants 
-------------------------------------------------------------------------------

squareSize = 20
lineLength = 35

-------------------------------------------------------------------------------
-- Data Base
-------------------------------------------------------------------------------
        

createListOfCells ::  Int -> Int -> [Cell]
createListOfCells x y 
    | x == squareSize && y == squareSize = [((squareSize, squareSize),(1,1,1,1))]
    | x == squareSize + 1                = (createListOfCells 0 (y+1))
    | otherwise                          = (createListOfCells (x+1) y) ++ [((x,y), (1, 1, 1, 1))]

orderList :: [Cell] -> [Cell]
orderList [((0,0),(n,s,w,e))] = [((0,0),(1,1,1,1))]
orderList list = (orderList (tail list)) ++ [head list]

--------------------------------------------------------------------------------
-- Prim function
--------------------------------------------------------------------------------

-- ([celulas conferidas], [celulas nao conferidas])
primFunction :: ([Cell], [Cell]) -> ([Cell], [Cell])
primFunction (listDone, []) = (listDone, [])
primFunction (checkList, unCheckList) = primFunction (openPath (returnRandomCell (createAllNeighbours checkList)) (checkList, unCheckList))


aux :: [Cell] -> (Coord, Coord) -> Bool
aux cell coord = length ([snd x | x <- cell, fst x == snd coord]) < 1      

-- create all neighbour`s coord from a list of cells
createAllNeighbours :: [Cell] -> [(Coord, Coord)]
createAllNeighbours cell =
  filter (\x -> aux cell x)
  (filter (\x -> fst (snd x) >= 0 && snd (snd x) >= 0 &&  fst (snd x) <= squareSize && snd (snd x) <= squareSize)
  (concat [[(fst a, (fst (fst a)+1, snd (fst a))), (fst a, (fst (fst a)-1, snd (fst a))), (fst a, (fst (fst a), snd (fst a)+1)), (fst a, (fst (fst a), snd (fst a)-1))] | a <- cell]))

-- escolhe aleatoriamente uma das tuplas da lista criada
returnRandomCell :: [(Coord, Coord)] -> (Coord, Coord)
returnRandomCell x = last (take (unsafePerformIO (randomRIO (1, length x))) x)

createPathEast :: Cell -> Cell
createPathEast ((x,y),(n,s,w,e)) = ((x,y),(n,s,w,0))

createPathSouth :: Cell -> Cell
createPathSouth ((x,y),(n,s,w,e)) = ((x,y),(n,0,w,e))

createPathNorth :: Cell -> Cell
createPathNorth ((x,y),(n,s,w,e)) = ((x,y),(0,s,w,e))

createPathWest :: Cell -> Cell
createPathWest ((x,y),(n,s,w,e)) = ((x,y),(n,s,0,e))

-- modifica os valores das paredes para criar o caminho no labirinto
openPath :: (Coord, Coord) -> ([Cell], [Cell]) -> ([Cell], [Cell])
openPath adjCells cellsList
-- open left cell
  | fst (fst adjCells) > fst (snd adjCells) = (delete oldCell (fst cellsList) ++ [createPathWest oldCell] ++ [createPathEast newCell],delete newCell (snd cellsList))
-- open right cell
  | fst (fst adjCells) < fst (snd adjCells) = (delete oldCell (fst cellsList) ++ [createPathEast oldCell] ++ [createPathWest newCell],delete newCell (snd cellsList))
-- open lower cell
  | snd (fst adjCells) < snd (snd adjCells) = (delete oldCell (fst cellsList) ++ [createPathSouth oldCell] ++ [createPathNorth newCell],delete newCell (snd cellsList))
-- open lower cell
  | snd (fst adjCells) > snd (snd adjCells) = (delete oldCell (fst cellsList) ++ [createPathNorth oldCell] ++ [createPathSouth newCell],delete newCell (snd cellsList))
 where
    oldCell = removeMaybe (find (\x -> (fst x)==(fst adjCells)) (fst cellsList))
    newCell = removeMaybe (find (\x -> (fst x)==(snd adjCells)) (snd cellsList))


removeMaybe :: Maybe Cell -> Cell
removeMaybe Nothing = ((0,0),(0,0,0,0))
removeMaybe (Just i) = i


-- doLader :: Cell -> Cell
-- doLader ((x,y),(n,s,w,e))
--     | x == (y+1)     = ((x,y),(n,s*0,w*0,e))
--     | x == y         = ((x,y),(n*0,s,w,e*0))
--     | otherwise      = ((x,y),(n,s*0,w*0,e))

-- pathLader :: [Cell] -> [Cell]
-- pathLader list = map doLader list

listOfCells :: [Cell]
listOfCells = orderList (createListOfCells 0 0)

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

svgBorder :: Int -> Int -> Int -> Int -> String
svgBorder x y w h= printf "<rect x='%d' y='%d' width='%d' height='%d' style='fill:none;stroke:black;stroke-width:5;opacity:1' />" x y w h

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensoes do retângulo e uma string com atributos de estilo
drawLine :: Int -> Int -> Int -> Int -> String
drawLine x1 y1 x2 y2 = printf "<line x1='%d' y1='%d' x2='%d' y2='%d' style='stroke:black; stroke-width:2'/>\n" x1 y1 x2 y2

svgCells :: Cell -> String 
svgCells ((x,y), (n,s,w,e)) =
  drawLine ((lineLength*(x+1))) ((lineLength*(y+1))) ((lineLength*(x+1))+(lineLength*n)) ((lineLength*(y+1)))++
  drawLine ((lineLength*(x+1))) ((lineLength*(y+1))+ (lineLength*s)) ((lineLength*(x+1))+ (lineLength*s)) ((lineLength*(y+1))+ (lineLength*s))++
  drawLine ((lineLength*(x+1))) ((lineLength*(y+1))) ((lineLength*(x+1))) ((lineLength*(y+1))+ (lineLength*w))++
  drawLine ((lineLength*(x+1))+ (lineLength*e)) ((lineLength*(y+1))) ((lineLength*(x+1))+ (lineLength*e)) ((lineLength*(y+1))+ (lineLength*e))


-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
    
  writeFile "T1.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ (svgBorder lineLength lineLength ((squareSize+1)*lineLength) ((squareSize+1)*lineLength))  ++ svgfigs ++ svgEnd
        svgfigs = concat $ map svgCells (fst (primFunction ([head listOfCells], tail listOfCells))) 
        (w,h) = (1000,1000) -- width, height da imagem SVG