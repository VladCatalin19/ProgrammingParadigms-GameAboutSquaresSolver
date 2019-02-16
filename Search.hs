{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Maybe as M
import qualified Data.List as L

{-
    Tipul unei nod utilizat în procesul de căutare. Se rețin următoarele
    informațiiȘ

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = Nod {state :: s, action :: a, parent :: (Node s a), depth :: Int}
              | NullNode
    deriving (Eq, Show)

{-
    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (Nod stare _ _ _) = stare
nodeState NullNode = undefined

{-
    Întoarce adâncimea stocată într-un nod.
-}
nodeDepth :: Node s a -> Int
nodeDepth (Nod _ _ _ inalt) = inalt
nodeDepth NullNode = undefined

{-
    Întoarce parintele stocat într-un nod.
-}
nodeParent :: Node s a -> Node s a
nodeParent (Nod _ _ parinte _) = parinte
nodeParent NullNode = undefined

{-
    Creează un nod pe baza unei perechi.
-}
pairToNode :: (a, s)        -- Perechea ce va fi stocata
           -> Node s a      -- Nodul parinte
           -> Int           -- Adancimea la care se afla nodul
           -> Node s a      -- Nodul rezultat
pairToNode (actiune, stare) parinte inalt = Nod stare actiune parinte inalt

{-
    Creează o pereche pe baza unui nod.
-}
nodeToPair :: Node s a -> (a, s)
nodeToPair (Nod st ac _ _) = (ac, st)
nodeToPair _ = undefined

{-
    Verifică dacă părintele unui nod este nul.
-}
isParentNull :: Node s a -> Bool
isParentNull (Nod _ _ NullNode _) = True
isParentNull _ = False

{-
    Compară euristicile a două stări.
-}
comparator :: (ProblemState s a, Ord s)
           => (a,s)
           -> (a,s)
           -> Ordering
comparator (_, s1) (_,s2) = compare (heuristic s1) (heuristic s2)

{-
    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Dacă se folosește euristica, se sorteaza succesorii unei stări crescător
    după euristicile stărilor.
-}
dls :: (ProblemState s a, Ord s)
    => [Node s a]         -- Stiva cu nodurile ce trebuie vizitate
    -> Bool               -- Pentru BONUS, `True` dacă utilizăm euristica
    -> Int                -- Adâncimea la care se află nodul curent
    -> Int                -- Adâncimea maximă de explorare
    -> [Node s a]         -- Nodurile deja vizitate
    -> [Node s a]         -- Lista de noduri
dls [] _ _ _ visited = visited

dls stack isHeur currentDepth maxDepth visited = 
    if currentDepth > maxDepth then
        -- Trece la următorul nod din stivă.
        dls stackRest isHeur (nodeDepth $ head stackRest) maxDepth visited
    else
        -- Verifică dacă nodul a fost deja vizitat.
        if or (map eqFirst visited) then
            -- Trece la următorul nod din stivă.
            dls stackRest isHeur (nodeDepth $ head stackRest) maxDepth visited
        else
            -- Adaugă succesorii nodului în stivă și marchează nodul curent ca
            -- fiind vizitat.
            dls (succsNodes ++ stackRest) isHeur (currentDepth + 1) maxDepth
                 (visited ++ [stackFirst])
      where
        stackFirst = head stack
        stackRest = tail stack
        eqFirst nod = (nodeState stackFirst) == (nodeState nod)
        succsPairs = 
            if isHeur then
                L.sortBy comparator $ successors (nodeState stackFirst)
            else
                successors (nodeState stackFirst)
        func (ac, st) acc = (pairToNode (ac, st) stackFirst (currentDepth+1)):acc
        succsNodes = foldr func [] succsPairs

{-
    Apelul principal al funcției care returnează lista nodurilor rezultate prin
    parcurgerea limitată în adâncime a spațiului stărilor, pornind de la starea
    dată ca parametru.
-}
limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs initState isHeur maxDep = dls [initNod] isHeur 0 maxDep []
    where initNod = (Nod initState undefined NullNode 0)

{-
    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}
iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening initState isHeur = (solution, length beforeSolLists
                                                  + (length inSolList))
    where
        -- Listă cu parcurgerile dfs pentru toate numerele naturale.
        dfsLists = foldr (\maxDep acc -> 
                         [limitedDfs initState isHeur maxDep] ++ acc) [] [0..]
        -- Listă care conține Nothing sau Just soluție în fucție de existența
        -- unei stări finale în lista cu parcurgeri în adâncime. 
        possibleSolutionList = foldr (\nodList acc2 -> 
            [L.find (\nod -> isGoal (nodeState nod)) nodList] ++ acc2) [] dfsLists
        -- Poziția în listă a stării finale.
        solutionIdx = (M.fromJust $ L.findIndex M.isJust possibleSolutionList) + 1
        -- Prima stare finală gasită.
        solution = M.fromJust $ M.fromJust (L.find M.isJust possibleSolutionList)
        -- Listă cu stările parcurse prin DFS înaintea parcurgerii care conține
        -- soluția.
        beforeSolLists = concat $ take (solutionIdx - 1) dfsLists
        -- Listă cu stările parcurse prin DFS din parcurgerea care a găsit soluția.
        inSolList = takeWhile (\(Nod st _ _ _) -> st /= stSol) 
                              $ dfsLists !! (solutionIdx - 1)
        (Nod stSol _ _ _) = solution

{-
    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
extractPath :: Node s a -> [(a, s)]
extractPath nod = reverse $ L.unfoldr f nod
    where
        f nod1 = 
            if isParentNull nod1 then
                Nothing
            else
                Just (nodeToPair nod1, nodeParent nod1)

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))