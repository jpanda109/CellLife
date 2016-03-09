import qualified Haste
import qualified Haste.DOM as DOM
import qualified Haste.Events as Events
import qualified Haste.Graphics.Canvas as Canvas
import qualified Data.Map as Map
import qualified Data.IORef as IORef

type Grid = Map.Map (Int, Int) Bool
type Rules = ([Bool], [Bool])

defaultRules =
    --([True | x <- [0..8]], [True | x <- [0..8]])
    ([x == 2 || x == 3 | x <- [0..8]], [x == 3 | x <- [0..8]])

isAlive :: Grid -> Int -> Int -> Bool
isAlive grid x y =
    case Map.lookup (x, y) grid of
      Nothing -> False
      Just r  -> r

aliveNeighbors :: Grid -> Int -> Int -> Int
aliveNeighbors grid x y =
    let coords = [(x+c1, y+c2) | c1 <- [-1..1], c2 <- [-1..1], c1 /= 0 || c2 /= 0] in
    foldl (\acc c -> if uncurry (isAlive grid) c then acc+1 else acc) 0 coords

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
    Canvas.fill $ Canvas.rect (f $ x*20, f $ y*20) (f $ x*20+20, f $ y*20+20)
    where f = fromIntegral

drawCell :: Grid -> Int -> Int -> Canvas.Picture ()
drawCell grid x y =
    if isAlive grid x y
      then Canvas.color (Canvas.RGB 0 0 0) (getRect grid x y)
      else Canvas.color (Canvas.RGB 255 255 255) (getRect grid x y)

drawGrid :: Canvas.Canvas -> Grid -> IO ()
drawGrid canvas grid =
    Canvas.render canvas $
      mapM_ (uncurry $ drawCell grid) $ Map.keys grid

makeGrid :: Int -> Int -> Grid
makeGrid w h =
    Map.fromList [((x, y), x == 10 || y == 10) | x <- [0..w-1], y <- [0..h-1]]


main :: IO()
main = do
  grid <- IORef.newIORef $ makeGrid 25 25
  Just canvas <- Canvas.getCanvasById "canvas"
  IORef.readIORef grid >>= drawGrid canvas
  Just nextStateButton <- DOM.elemById "nextStateButton"
  Events.onEvent nextStateButton Events.Click $ \_ -> do
    IORef.modifyIORef grid $ \st ->
      nextState st defaultRules
    IORef.readIORef grid >>= drawGrid canvas
    return ()
  return ()

