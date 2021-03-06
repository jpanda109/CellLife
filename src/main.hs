import qualified Haste
import qualified Haste.DOM as DOM
import qualified Haste.Events as Events
import qualified Haste.Graphics.Canvas as Canvas
import qualified Data.Map as Map
import qualified Data.IORef as IORef
import qualified Control.Monad as Monad

type Grid = Map.Map (Int, Int) Bool
type Rules = ([Bool], [Bool])
data GridState = GridState { grids :: [Grid]
                           , curIndex :: Int
                           , curGrid :: Grid
                           }

incrementGridState :: GridState -> Int -> GridState
incrementGridState state i =
    if (curIndex state)+i >= length (grids state) || (curIndex state)+i < 0
      then state
      else let j = (curIndex state) + i in
        GridState { grids = grids state
                  , curIndex = j
                  , curGrid = (grids state) !! j
                  }

setGridState :: GridState -> Int -> GridState
setGridState state i =
    if i < length (grids state) && i >= 0
      then GridState { grids = grids state
                     , curIndex = i
                     , curGrid = (grids state) !! i
                     }
      else state

defaultRules :: ([Bool], [Bool])
defaultRules =
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

switchCell :: Grid -> Int -> Int -> Grid
switchCell grid x y =
    let switch ((x2, y2), b) = if x2 /= x || y2 /= y 
                                 then ((x2, y2), b) 
                                 else ((x2, y2), not b) in
    Map.fromList $ map switch $ Map.toList grid

calculateStates :: Int -> Grid -> [Grid]
calculateStates n grid
    | n <= 0    = []
    | otherwise = let next = nextState grid defaultRules in
      grid : calculateStates (n-1) next

drawGrid :: Canvas.Canvas -> Grid -> IO ()
drawGrid canvas grid =
    Canvas.render canvas $
      mapM_ (uncurry $ drawCell grid) $ Map.keys grid

makeGrid :: Int -> Int -> Grid
makeGrid w h =
    Map.fromList [((x, y), x == 10 || y == 10) | x <- [0..w-1], y <- [0..h-1]]

makeGridState :: Int -> Int -> GridState
makeGridState w h =
    let grid = makeGrid 25 25 in
      GridState { grids = calculateStates 101 grid
                , curIndex = 0
                , curGrid = grid
                }

playLoop :: Canvas.Canvas -> IORef.IORef Bool -> IORef.IORef GridState -> IO()
playLoop canvas isPlayingRef gridStateRef = do
    isPlaying <- IORef.readIORef isPlayingRef
    Monad.when isPlaying $ do
      gridState <- IORef.readIORef gridStateRef
      IORef.modifyIORef gridStateRef $ \st ->
        incrementGridState st 1
      drawGrid canvas (curGrid gridState)
      Haste.setTimer (Haste.Once 500) $ playLoop canvas isPlayingRef gridStateRef
      return ()

main :: IO()
main = do
    gridStateRef <- IORef.newIORef $ makeGridState 25 25
    Just canvas <- Canvas.getCanvasById "canvas"
    gridState <- IORef.readIORef gridStateRef
    drawGrid canvas $ curGrid gridState
    Just nextStateButton <- DOM.elemById "h_nextStateButton"
    Events.onEvent nextStateButton Events.Click $ \_ -> do
      IORef.modifyIORef gridStateRef $ \st ->
        incrementGridState st 1
      gridState <- IORef.readIORef gridStateRef
      drawGrid canvas (curGrid gridState)
    {-
    Events.onEvent canvas Events.Click $ \(Events.MouseData (x, y) _ _) ->
      let d20 q = quot q 20 in do
        IORef.modifyIORef gridRef $ \st ->
          switchCell st (d20 x) (d20 y)
        IORef.readIORef gridRef >>= drawGrid canvas
    -}
    Just stateInput <- DOM.elemById "h_stateInput"
    Just stateRange <- DOM.elemById "h_stateRange"
    Events.onEvent stateRange Events.MouseMove $ \(Events.MouseData _ b _) ->
      case b of
        Just Events.MouseLeft -> do
          p <- DOM.getProp stateRange "value"
          DOM.setProp stateInput "value" p
          gridState <- IORef.readIORef gridStateRef
          drawGrid canvas ((grids gridState)!! (read p))
          IORef.modifyIORef gridStateRef $ \st ->
            setGridState st (read p)
        _ -> return ()
    Just calculateButton <- DOM.elemById "h_calculateButton"
    Events.onEvent calculateButton Events.Click $ \_ -> do
      IORef.modifyIORef gridStateRef $ \st ->
        GridState { grids = calculateStates 101 (curGrid st)
                  , curIndex = 0
                  , curGrid = (curGrid st)
                  }
      gridState <- IORef.readIORef gridStateRef
      drawGrid canvas (curGrid gridState)
    Just playButton <- DOM.elemById "h_playButton"
    isPlayingRef <- IORef.newIORef False
    Events.onEvent playButton Events.Click $ \_ -> do
      IORef.modifyIORef isPlayingRef $ \st -> not st
      isPlaying <- IORef.readIORef isPlayingRef
      Monad.when isPlaying $ playLoop canvas isPlayingRef gridStateRef
    return ()

