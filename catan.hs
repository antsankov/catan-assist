import Data.Tuple.Select

type Settlement = [Coord]

type Coord = (Int, Int)
data Resource = Sheep | Brick | Ore | Wood | Wheat
data Tile = Tile { loc :: Coord
                 , resource :: Resource
                 , score :: Int
                 }

coordList :: [Coord]
coordList = [           ( 0,2),( 1,2),( 2,2)
            ,       ( -1, 1),( 0, 1),( 1,1),( 2,1)
            ,   ( -2, 0),( -1,0),( 0,0),( 1,0),( 2,0)
            ,       ( -2, -1),( -1,-1),( 0,-1),( 1,-1)
            ,           ( -2,-2),( -1,-2), (0,-2)
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

main = 
    print $ borders (0, 0)
