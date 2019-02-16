{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M
import Data.Char

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Square Color Heading | Circle Color | Arrow Heading | NullObj
    deriving (Eq, Ord)

{-
    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show (Square color heading) = (toUpper $ head $ show color) : (show heading)
    show (Circle color) = [toLower $ head $ show color]
    show (Arrow heading) = show heading
    show NullObj = "null"

{-
    Un nivel al jocului.
-}
data Level = Lev (M.Map Position [Object])
    deriving (Eq, Ord)

{-
    Reprezentarea textuală a unei celule dintr-un nivel.
-}
showCell :: [Object] -> String
showCell [NullObj, NullObj, NullObj] = "   "

showCell [(Square color heading),   NullObj,        NullObj] = 
    show (Square color heading) ++ " "

showCell [NullObj,                  (Circle color), NullObj] = 
    "  " ++ show (Circle color)

showCell [NullObj,                  NullObj,        (Arrow heading)] = 
    "  " ++ show (Arrow heading)

showCell [(Square color1 heading),  (Circle color2), NullObj] = 
    show (Square color1 heading) ++ show (Circle color2)

showCell [(Square color heading1),  NullObj,        (Arrow heading2)] = 
    show (Square color heading1) ++ show (Arrow heading2)

showCell [NullObj,                  (Circle color), (Arrow heading)] = 
    show (Arrow heading) ++ show (Circle color)

showCell _ = "UwU"

{-
    Calculează coordonata x cu valoare minimă dintr-o lista de tip poziții.
-}
getXMin :: [Position] -> Int
getXMin list = foldl f (maxBound :: Int) list
    where f minEl (x, _) = min minEl x

{-
    Calculează coordonata x cu valoare maximă dintr-o lista de tip poziții.
-}
getXMax :: [Position] -> Int
getXMax list = foldl f (minBound :: Int) list
    where f maxEl (x, _) = max maxEl x

{-
    Calculează coordonata y cu valoare minimă dintr-o lista de tip poziții.
-}
getYMin :: [Position] -> Int
getYMin list = foldl f (maxBound :: Int) list
    where f minEl (_, y) = min minEl y

{-
    Calculează coordonata y cu valoare maximă dintr-o lista de tip poziții.
-}
getYMax :: [Position] -> Int
getYMax list = foldl f (minBound :: Int) list
    where f maxEl (_, y) = max maxEl y

{-
    Creează un string cu elementele de pe o linie din tabela de joc.
-}
makeRow :: Level -> Int -> String
makeRow (Lev levMap) x = init (foldl f "" [(x, y') | y' <- [yMin .. yMax]])
    where
        f = (\acc (x1, y1) ->
            if elem (x1, y1) (M.keys levMap) then
                acc ++ showCell (M.findWithDefault [] (x1, y1) levMap) ++ "|"
            else
                acc ++ "   |")
        yMin = getYMin (M.keys levMap)
        yMax = getYMax (M.keys levMap)

{-
    Reprezetarea textuală a unui nivel.
-}
instance Show Level where
    show (Lev levMap) = init (foldl f [] [xMin .. xMax])
        where
            f = (\acc x ->
                acc ++ makeRow (Lev levMap) x ++ "\n")

            xMin = getXMin (M.keys levMap)
            xMax = getXMax (M.keys levMap)

-------------------------------------------------------------------------------

{-
    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = Lev M.empty

{-
    Lista de obiecte vidă, fără obiecte.
-}
emptyList :: [Object]
emptyList = [NullObj, NullObj, NullObj]

{-
    Pune un pătrat pe prima pozitie dintr-o listă de obiecte.
-}
putSquareInList :: Object -> [Object] -> [Object]
putSquareInList square objLst = square : tail objLst

{-
    Pune obiectul nul pe pozitia pătratului.
-}
removeSquareFromList :: [Object] -> [Object]
removeSquareFromList objLst = putSquareInList NullObj objLst

{-
    Returnează obiectul de pe prima pozitie a listei de obiecte.
-}
getSquareFromList :: [Object] -> Object
getSquareFromList objLst = head objLst

{-
    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading position (Lev levMap) = Lev (M.insertWith f position
                                                                  insLst levMap)
    where f _ objLst = (putSquareInList s objLst)
          s = (Square color heading)
          insLst = [s, NullObj, NullObj]

{-
    Sterge un pătrat de la poziția precizată din nivel.
-}
removeSquare :: Position -> Level -> Level
removeSquare position (Lev levMap) = Lev (M.update f position levMap)
    where f objLst = if (removeSquareFromList objLst) == emptyList then Nothing
                       else Just (removeSquareFromList objLst)

-------------------------------------------------------------------------------

{-
    Pune un cerc pe pozitia a doua dintr-o listă de obiecte.
-}
putCircleInList :: Object -> [Object] -> [Object]
putCircleInList circle objLst = init objLst ++ [circle]

{-
    Pune obiectul nul pe pozitia cercului.
-}
removeCircleFromList :: [Object] -> [Object]
removeCircleFromList objLst = putCircleInList NullObj objLst

{-
    Returnează obiectul de pe a doua pozitie a listei de obiecte.
-}
getCircleFromList :: [Object] -> Object
getCircleFromList objLst = head (tail objLst)

{-
    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color position (Lev levMap) = Lev (M.insertWith f position insLst
                                                          levMap)
    where f _ objLst = (putCircleInList c objLst)
          c = (Circle color)
          insLst = [NullObj, c, NullObj]

-------------------------------------------------------------------------------

{-
    Pune o sageată pe pozitia a treia dintr-o listă de obiecte.
-}
putArrowInList :: Object -> [Object] -> [Object]
putArrowInList arrow objLst = head objLst : arrow : [last objLst]

{-
    Pune obiectul nul pe pozitia săgeții.
-}
removeArrowFromList :: [Object] -> [Object]
removeArrowFromList objLst = putArrowInList NullObj objLst

{-
    Returnează obiectul de pe a treia pozitie a listei de obiecte.
-}
getArrowFromList :: [Object] -> Object
getArrowFromList objLst = last objLst

{-
    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading position (Lev levMap) = Lev (M.insertWith f position insLst
                                                           levMap)
    where f _ objLst = (putArrowInList a objLst)
          a = (Arrow heading)
          insLst = [NullObj, NullObj, a]

-------------------------------------------------------------------------------
--                                  MOVE
-------------------------------------------------------------------------------

{-
    Calculează poziția următoare după efectuarea unei mutări.
-}
getNextPos :: Position -> Heading -> Position
getNextPos (x, y) heading 
    | heading == North = (x-1, y)
    | heading == South = (x+1, y)
    | heading == East  = (x, y+1)
    | heading == West  = (x, y-1)
    | otherwise = undefined

{-
    Șterge pătartul de pe prima poziție din nivel și îl adaugă pe a doua pozie
    din același nivel.
-}
moveSquareToPos :: Level -> Position -> Position -> Level
moveSquareToPos (Lev levMap) pos1 pos2 = 
    addSquare color heading pos2 (Lev levMapAfterRem)
    where
        (Lev levMapAfterRem) = removeSquare pos1 (Lev levMap)
        (Square color headingS) = getSquareFromList (M.findWithDefault emptyList
                                                                   pos1 levMap)
        arrow = getArrowFromList (M.findWithDefault emptyList pos2 levMap)
        (Arrow headingA) = arrow
        heading =
            if arrow == NullObj then
                headingS
            else
                headingA

{-
    Mută pătratul de la poziția precizată în direcția respectivă.
    Dacă există 1 sau mai multe pătrate lipite de pătratul dat ca parametru în
    direcția în care se doreste mutarea, se vor muta și acestea spre direcția
    indicată.
-}
moveSquares :: Level        -- Nivelul inițial
            -> Position     -- Poziția pătratului
            -> Heading      -- Direcția spre care se va muta pătratul
            -> Level        -- Nivelul final
moveSquares (Lev levMap) position heading =
    if getSquareFromList (M.findWithDefault emptyList nextPos levMap) == NullObj
    then
        (moveSquareToPos (Lev levMap) position nextPos)
    else
        (moveSquareToPos nextLev position nextPos)
    where
        nextPos = getNextPos position heading
        nextLev = moveSquares (Lev levMap) nextPos heading

{-
    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move position (Lev levMap) = 
    if square == NullObj then
        (Lev levMap)
    else
        moveSquares (Lev levMap) position heading
    where 
        square = getSquareFromList (M.findWithDefault emptyList position levMap)
        (Square _ heading) = square

-------------------------------------------------------------------------------
--                              SOLVE LEVEL
-------------------------------------------------------------------------------

{-
    Returnează o hartă cu toate pozițiile care conțin pătrate.
-}
getAllSquares :: Level -> M.Map Position [Object]
getAllSquares (Lev levMap) = M.filter f levMap
    where
        f = (\objLst -> getSquareFromList objLst /= NullObj)

{-
    Returnează o hartă cu toate pozițiile care conțin cercuri.
-}
getAllCircles :: Level -> M.Map Position [Object]
getAllCircles (Lev levMap) = M.filter f levMap
    where
        f = (\objLst -> getCircleFromList objLst /= NullObj)

{-
    Returnează poziția care conține un cerc de o anumită culoare.
-}
getCircleCertainColorPos :: [(Position, Object)] -> Color -> Position
getCircleCertainColorPos list color = fst $ head $ filter f list
    where f (_, Circle colorC) = color == colorC

{-
    Verifică dacă culoarea unui pătrat coincide cu culoarea unui cerc.
-}
checkIfColorsMatch :: [Object] -> Bool
checkIfColorsMatch [(Square colorS _), (Circle colorC), _] = colorS == colorC
checkIfColorsMatch _ = False

{-
    Returnează distanța intre 2 puncte.
-}
getDistance :: Position -> Position -> Int
getDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

{-
    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where

    {-
        Întoarece o listă de forma (Position, Level) unde Position este po-
        ziția de unde s-a mutat un pătrat, iar Level nivelul rezultat prin
        aplicarea mutării respective.
    -}
    successors level = foldr f [] $ M.keys $ getAllSquares level
        where
            f pos acc = (pos, move pos level) : acc

    {-
        Verifică dacă toate pozițiile cu pătrate conțin cercuri de aceeași
        culoare cu pătratul respectiv.
    -}
    isGoal (Lev levMap) = and (map f $ M.keys $ getAllSquares (Lev levMap))
        where
            f pos = checkIfColorsMatch $ objLst pos
            objLst pos = M.findWithDefault emptyList pos levMap

    {-
        Pentru fiecare pătrat se calcuză distanța aproximativă până la cercul
        de culoare respectivă.

        Returnează suma distanțelor pentru fiecare pătrat din nivel.
    -}
    heuristic (Lev levMap) = foldr f 0 squares
        where
            f (pos, (Square color _)) acc = 
                acc + (getDistance pos $ getCircleCertainColorPos circles color)
            squares = foldr fs [] $ M.toList $ getAllSquares (Lev levMap)
            fs = (\(pos, objLst) acc -> (pos, getSquareFromList objLst) : acc)
            circles = foldr fc [] $ M.toList $ getAllCircles (Lev levMap)
            fc = (\(pos, objLst) acc -> (pos, getCircleFromList objLst) : acc)
