import Data.List (sortOn)

type Estado = (Int, Int)
type Nodo = Estado
type Costo = Int

capA, capB :: Int
capA = 5
capB = 3

vecinos :: Estado -> [(Estado, Costo)]
vecinos (a,b) =
  [ ((capA, b), 1)
  , ((a, capB), 1)
  , ((0, b), 1)
  , ((a, 0), 1)
  , (transferirAB (a,b), 1)
  , (transferirBA (a,b), 1)
  ]

transferirAB, transferirBA :: Estado -> Estado
transferirAB (a, b) =
  let espacioB = capB - b
      transferir = min a espacioB
  in (a - transferir, b + transferir)

transferirBA (a, b) =
  let espacioA = capA - a
      transferir = min b espacioA
  in (a + transferir, b - transferir)

heuristica :: Estado -> Costo
heuristica (a, b) = min (abs (a - 4)) (abs (b - 4))

-- Algoritmo A*
aStar :: (Nodo -> [(Nodo, Costo)]) -> (Nodo -> Costo) -> Nodo -> Nodo -> [(Nodo, Costo)]
aStar sucesores heuristica inicio meta = buscar [(inicio, 0)] []
  where
    buscar [] _ = []
    buscar ((nodo, costo):cola) visitados
      | nodo == meta = [(nodo, costo)]
      | nodo `elem` visitados = buscar cola visitados
      | otherwise =
          let nuevos = [(v, costo + c) | (v, c) <- sucesores nodo, v `notElem` visitados]
              ordenados = sortOn (\(v, c) -> c + heuristica v) (cola ++ nuevos)
          in (nodo, costo) : buscar ordenados (nodo : visitados)

main :: IO ()
main = print (aStar vecinos heuristica (0,0) (4,0))
