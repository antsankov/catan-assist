import Data.Tuple.Select

import Data.Map


type Coord = (Int, Int)
type Settlement = [Coord]

data Resource = Sheep | Brick | Ore | Wood | Wheat deriving (Show)
data Tile = Tile { resource :: Resource
                 , score :: Int
                 }
                 deriving (Show)

coordList :: [Coord]
coordList = [           ( 0,2),( 1,2),( 2,2)
            ,       ( -1, 1),( 0, 1),( 1,1),( 2,1)
            ,   ( -2, 0),( -1,0),( 0,0),( 1,0),( 2,0)
            ,       ( -2, -1),( -1,-1),( 0,-1),( 1,-1)
            ,           ( -2,-2),( -1,-2), (0,-2)
            ]

testBoard :: [(Coord, Tile)]
testBoard = [
                ((0,2), Tile Brick 3)
            ,   ((1,2), Tile Ore 2)
            ]

-- Given a hex coord, give the hexs bordered by all possible settlements.
borders :: Coord -> [Settlement]
borders hex =
    [    -- 0 deg
         [hex, (sel1 hex, sel2 hex + 1), (sel1 hex +1, sel2 hex+1)]
         -- 60 deg
    ,    [hex, (sel1 hex +1, sel2 hex+1), (sel1 hex +1, sel2 hex)]
         -- 120 deg
    ,    [hex, (sel1 hex +1, sel2 hex), (sel1 hex, sel2 hex - 1)]
         -- 180 deg
    ,    [hex, (sel1 hex, sel2 hex - 1), (sel1 hex - 1, sel2 hex - 1)]
         -- 240 deg
    ,    [hex, (sel1 hex - 1, sel2 hex - 1), (sel1 hex - 1, sel2 hex)]
         -- 300 deg
    ,    [hex, (sel1 hex - 1, sel2 hex), (sel1 hex, sel2 hex + 1)]
    ]

-- TODO: Generate tiles with resources, make a hashmap from tile to coord, iterate through borders to create scores.
mapTest :: [Int] -> [Int] 
mapTest settlement =
    Prelude.map (+1) settlement


getFst :: (Int, Int) -> Int
getFst tup =
    fst tup

calculateValue :: Coord -> Maybe Tile
calculateValue loc =
    let 
        board = fromList(testBoard) 
    in
        Data.Map.lookup (0,2) board

main = 
    -- print $ borders (0, 0)
    print $ calculateValue (1, 2)
    -- print $ getFst (10,11)
