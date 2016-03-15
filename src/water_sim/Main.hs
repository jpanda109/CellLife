import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.IORef as IORef
import qualified Haste.DOM as DOM
import qualified Haste.Events as Events
import qualified Haste.Graphics.Canvas as Canvas

data Cell = Ground | Water Int
type Grid = Map.Map (Int, Int) Cell
type Coord = (Int, Int)

getCell :: Grid -> Coord -> Cell
getCell grid coord =
    Maybe.fromMaybe Ground (Map.lookup coord grid)

shiftWater :: Grid -> Coord -> Coord -> Grid
shiftWater grid src dst =
    let srcCell = getCell grid src
        dstCell = getCell grid dst
    in case (srcCell, dstCell) of
         (Water srcN, Water dstN) ->
           Map.insert dst (Water $ dstN+1) . Map.insert src (Water $ srcN-1) $ grid
         (_, _)                   -> grid

spread :: Grid -> Coord -> Grid
spread grid coord =
    case (getCell grid left, getCell grid cur, getCell grid right) of
      (Water l, Water c, Water r) 
        | c>l          -> shiftWater grid cur left
        | c>r          -> shiftWater grid cur right
      (Ground, Water c, Water r) | c>r -> shiftWater grid cur right
      (Water l, Water c, Ground) | c>l -> shiftWater grid cur left
      (_, _, _) -> grid
    where (x, y) = coord
          left = (x-1, y)
          right = (x+1, y)
          cur = (x, y)

changeState :: Grid -> Coord -> Grid
changeState grid coord =
    let (x, y) = coord
        down = (x, y-1)
    in case getCell grid coord of
         Ground -> grid
         Water n -> 
           case getCell grid down of
             Ground -> grid
             Water nd ->
               if nd < n && nd < 3
                 then shiftWater grid coord down
                 else spread grid coord 

nextState :: Grid -> Grid
nextState grid =
    foldl changeState grid $ Map.keys grid

drawCell :: Grid -> Coord -> Canvas.Picture ()
drawCell grid coord =
    let (x, y) = coord
        s = 20
        h = 200
        rect = Canvas.rect (f $ h-x*s, f $ h-y*s) (f $ h-(x+1)*s, f $ h-(y+1)*s)
          where f = fromIntegral
    in do
      case getCell grid coord of
        Ground -> Canvas.color (Canvas.RGB 0 0 0) $ Canvas.fill rect
        Water n -> Canvas.color (Canvas.RGBA 0 0 255 (fromIntegral n / 3.0)) $ Canvas.fill rect
      Canvas.color (Canvas.RGB 0 0 0) $ Canvas.stroke rect

drawGrid :: Canvas.Canvas -> Grid -> IO ()
drawGrid canvas grid =
    Canvas.render canvas $
      mapM_ (drawCell grid) $ Map.keys grid

initialGrid :: Grid
initialGrid =
    let f x y
            | y == 5    = Ground
            | x == 5    = Ground
            | x == 4    = Water 3
            | otherwise = Water 0
    in
    Map.fromList [((x, y), f x y) | x <- [0..9], y <- [0..9]]

main :: IO ()
main = do
    gridRef <- IORef.newIORef initialGrid
    Just canvas <- Canvas.getCanvasById "h_canvas"
    IORef.readIORef gridRef >>= drawGrid canvas
    Just nextStateButton <- DOM.elemById "h_nextStateButton"
    Events.onEvent nextStateButton Events.Click $ \_ -> do
      IORef.modifyIORef gridRef nextState
      IORef.readIORef gridRef >>= drawGrid canvas
    return ()
