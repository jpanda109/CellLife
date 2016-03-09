import qualified Haste
import qualified Haste.Graphics.Canvas as Canvas
import qualified Data.Map as Map
import qualified Data.List as List

type Grid = Map.Map (Int, Int) Bool
type Rules = ([Bool], [Bool])
type Coord = (Int, Int)

isAlive :: Grid -> Int -> Int -> Bool
isAlive grid x y =
    case Map.lookup (x, y) grid of
      Nothing -> False
      Just r  -> r

aliveNeighbors :: Grid -> Int -> Int -> Int
aliveNeighbors grid x y =
    let coords = [(x+c1, y+c2) | c1 <- [-1..1], c2 <- [-1..1], c1 /= x && c2 /= y] in
    foldl (\acc c -> if isAlive grid x y then acc+1 else acc) 0 coords

shouldLive :: Grid -> Rules -> Int -> Int -> Bool
shouldLive grid rules x y =
    let rule = if isAlive grid x y
                 then fst rules 
                 else snd rules
    in rule !! aliveNeighbors grid x y

nextState :: Grid -> Rules -> Grid
nextState grid rules =
    let shouldLiveAt = shouldLive grid rules
    in
      Map.fromList . map (\c -> (c, uncurry shouldLiveAt c)) $ Map.keys grid

getRect :: Grid -> Int -> Int -> Canvas.Picture ()
getRect grid x y =
    Canvas.fill $ Canvas.rect (f $ x*5, f $ y*5) (f $ x*5+5, f $ y*5+5)
    where f = fromIntegral

drawCell :: Grid -> Int -> Int -> Canvas.Picture ()
drawCell grid x y =
    if isAlive grid x y
      then Canvas.color (Canvas.RGB 255 0 0) (getRect grid x y)
      else Canvas.color (Canvas.RGB 0 0 0) (getRect grid x y)

drawGrid :: Canvas.Canvas -> Grid -> IO ()
drawGrid canvas grid =
    Canvas.render canvas $
      mapM_ (uncurry $ drawCell grid) $ Map.keys grid

makeGrid :: Int -> Int -> Grid
makeGrid w h =
    Map.fromList [((x, y), False) | x <- [0..w], y <- [0..h]]


main :: IO()
main = do
  Just canvas <- Canvas.getCanvasById "canvas"
  drawGrid canvas (makeGrid 10 10)

