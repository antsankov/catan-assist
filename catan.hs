-- Iterate through all possible settlments, calculating their scores.
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

-- Extract the aggregate score for an array of tiles.
calcSettlementValue :: [Coord] -> Int
calcSettlementValue tiles =
    sum $ Prelude.map extractScore tiles

-- Given a coordinate, get the score from it.
extractScore :: Coord -> Int
extractScore coord =
    case Data.Map.lookup coord testBoard of
        Nothing -> 0
        Just tile -> score tile

-- DEPRECATED Given a settlement, return the tiles it touches.
calcSettlementTiles :: Settlement -> [Maybe Tile]
calcSettlementTiles settlement =
    Prelude.map (\loc -> Data.Map.lookup loc testBoard) settlement


main = 
    -- print $ possibleSettlements (0, 1)
    print $ calcSettlementValue [(0,1),(0,2),(1,2)]

