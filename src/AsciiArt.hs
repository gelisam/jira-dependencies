module AsciiArt where


type Row = String
type Layer = [String]
type AsciiArt = [[String]]

-- layer (0 is top-most),
-- x (increases towards the right),
-- y (increases towards the bottom)
type Coord = (Int, Int, Int)


emptyChar :: Char
emptyChar = ' '

emptyRow :: Row
emptyRow = ""

emptyLayer :: Layer
emptyLayer = []

emptyAsciiArt :: AsciiArt
emptyAsciiArt = []


overChar :: Char -> Char -> Char
overChar ' ' y = y
overChar x   _ = x

overRow :: Row -> Row -> Row
overRow []     y      = y
overRow x      []     = x
overRow (x:xs) (y:ys) = overChar x y : overRow xs ys

overLayer :: Layer -> Layer -> Layer
overLayer []     y      = y
overLayer x      []     = x
overLayer (x:xs) (y:ys) = overRow x y : overLayer xs ys

overAsciiArt :: AsciiArt -> AsciiArt -> AsciiArt
overAsciiArt []     y      = y
overAsciiArt x      []     = x
overAsciiArt (x:xs) (y:ys) = overLayer x y : overAsciiArt xs ys


renderAsciiArt :: AsciiArt -> Layer
renderAsciiArt = foldr overLayer emptyLayer

printLayer :: Layer -> IO ()
printLayer = mapM_ putStrLn

printAsciiArt :: AsciiArt -> IO ()
printAsciiArt = printLayer . renderAsciiArt


layerAt :: Coord -> Layer -> AsciiArt
layerAt (l,x,y) layer = replicate l emptyLayer
                     ++ [ replicate y emptyRow
                       ++ fmap (replicate x emptyChar ++) layer
                        ]

-- |
-- >>> printAsciiArt $ stringAt (0,2,0) "|" `overAsciiArt` stringAt (0,0,0) "-----"
-- --|--
-- >>> printAsciiArt $ stringAt (1,0,0) "-----" `overAsciiArt` stringAt (0,2,0) "|"
-- --|--
stringAt :: Coord -> String -> AsciiArt
stringAt c s = layerAt c [s]

-- |
-- >>> printAsciiArt $ hlineAt (0,0,0) 5
-- -----
hlineAt :: Coord -> Int -> AsciiArt
hlineAt c w = stringAt c (replicate w '-')

-- |
-- >>> printAsciiArt $ vlineAt (0,0,0) 2
-- |
-- |
vlineAt :: Coord -> Int -> AsciiArt
vlineAt c h = layerAt c (replicate h "|")

-- |
-- >>> printAsciiArt $ boxAt (0,0,0) 3 1
-- ,---,
-- |   |
-- '---'
boxAt :: Coord -> Int -> Int -> AsciiArt
boxAt c w h = layerAt c
            $ ["," ++ replicate w '-' ++ ","]
           ++ replicate h
              ("|" ++ replicate w ' ' ++ "|")
           ++ ["'" ++ replicate w '-' ++ "'"]
