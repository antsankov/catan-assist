-- TODO iterate through list of maybe tiles to create an aggregate score

import Data.Tuple.Select
import Data.Map


type Coord = (Int, Int)
type Settlement = [Coord]

data Resource = Sheep | Brick | Ore | Wood | Wheat deriving (Show)
data Tile = Tile { resource :: Resource
                 , score :: Int
                 } deriving (Show)


coordList :: [Coord]
coordList = [           ( 0,2),( 1,2),( 2,2)
            ,       ( -1, 1),( 0, 1),( 1,1),( 2,1)
            ,   ( -2, 0),( -1,0),( 0,0),( 1,0),( 2,0)
            ,       ( -2, -1),( -1,-1),( 0,-1),( 1,-1)
            ,           ( -2,-2),( -1,-2), (0,-2)
            ]

testBoard :: Map Coord Tile 
testBoard = fromList[
                ((0,2), Tile Brick 3)
            ,   ((1,2), Tile Ore 2)
            ,   ((-1, 1), Tile Wheat 3)
            ,   ((0, 1), Tile Sheep 5)
            ]

-- Given a hex coord, give the hexs bordered by all possible settlements.
possibleSettlements :: Coord -> [Settlement]
possibleSettlements hex =
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

-- Given a settlement, return the tiles it touches.
calcSettlementTiles :: Settlement -> [Maybe Tile]
calcSettlementTiles settlement =
    Prelude.map (\loc -> Data.Map.lookup loc testBoard) settlement


-- Extract the aggregate score for an array of tiles.
calcSettlementValue :: [Maybe Tile] -> Int
calcSettlementValue tiles =
    fold (+) tiles . score

main = 
    -- print $ possibleSettlements (0, 1)
    print $ calcSettlementTiles [(0,1),(0,2),(1,2)]

