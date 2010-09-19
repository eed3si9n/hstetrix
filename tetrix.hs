module Main where

import System.Random
import qualified Data.Map as Map
import Data.Maybe
import Control.Exception
import Control.Monad.State
import qualified UI.HSCurses.Curses as C

defaultBoardWidth = 9
defaultBoardHeight = 20
origin = (0, 0)
nextBoardHeight = 5
defaultEventTimeoutMsec = 100
tickFrequency = 7 -- ticks every 0.7 seconds

data Cell = Tee | Bar | Box | El | Jay | Es | Zee

data Block = Block {
  blockPos :: (Int, Int),
  blockLocals :: [(Double, Double)],
  blockKind :: Cell
}

type CellMap = Map.Map (Int, Int) Cell

data Board = Board {
  boardSize :: (Int, Int),
  boardCells :: CellMap
}

data Game = Game {
  gameBlock :: Block,
  gameNextBlock :: Block,
  gameBoard :: Board,
  gameGen :: StdGen
}

data TetrixVar = TetrixVar {
  varGame1 :: Game,
  varGame2 :: Game
}

type TetrixState = StateT TetrixVar IO

runTetrix :: Int -> TetrixState a -> IO a
runTetrix seed m = evalStateT m (initState seed)

boardWidth board = fst (boardSize board)

boardHeight board = snd (boardSize board)

initBoard width height =
    Board {
      boardSize = (width, height),
      boardCells = Map.empty
    }
    
initBlock kind =
    Block {
      blockPos = (3, 3),
      blockLocals = initLocals kind,
      blockKind = kind
    }

initLocals :: Cell -> [(Double, Double)]
initLocals Tee = [(0.0, 0.0), (-1.0, 0.0), (1.0, 0.0), (0.0, 1.0)]
initLocals Bar = [(0.0, -1.5), (0.0, -0.5), (0.0, 0.5), (0.0, 1.5)]
initLocals Box = [(-0.5, 0.5), (0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)]
initLocals El  = [(0.0, 0.0), (0.0, 1.0), (0.0, -1.0), (1.0, -1.0)]
initLocals Jay = [(0.0, 0.0), (0.0, 1.0), (0.0, -1.0), (-1.0, -1.0)]
initLocals Es  = [(-0.5, 0.0), (0.5, 0.0), (-0.5, 1.0), (0.5, -1.0)]
initLocals Zee = [(-0.5, 0.0), (0.5, 0.0), (-0.5, -1.0), (0.5, 1.0)]

newGame block nextBlock board gen =
    Game {
      gameBlock = block,
      gameNextBlock = nextBlock,
      gameBoard = board,
      gameGen = gen    
    }

newState game1 game2 =
    TetrixVar {
      varGame1 = game1,
      varGame2 = game2 
    }
    
setVarGen gen game =
    newGame (gameBlock game) (gameNextBlock game) (gameBoard game) gen

initState seed = newState (initGame seed) (initGame seed)

initGame seed =
    newGame block nextBlock board gen''
    where board = initBoard defaultBoardWidth defaultBoardHeight
          (kind, gen') = randomKind $ mkStdGen seed
          (nextKind, gen'') = randomKind gen'
          block = initBlock kind `moveTo` loadPos board
          nextBlock = initBlock nextKind    

randomKind gen =
    (head $ drop value cells, gen')
    where (value, gen') = randomR (0, length cells - 1) gen
          cells = [Tee, Bar, Box, El, Jay, Es, Zee]
          
randomKindTransition game =
    (kind, setVarGen gen' game)
    where (kind, gen') = randomKind (gameGen game)

blockCells :: Block -> [(Int, Int)]
blockCells block =
    map (\x -> floorPair x `addPair` blockPos block) $ blockLocals block

loadPos board = ((boardWidth board) `div` 2, (boardHeight board) - 2)

moveTo block pos =
    Block {
      blockPos = pos,
      blockLocals = blockLocals block,
      blockKind = blockKind block
    }

moveBy delta block = block `moveTo` (delta `addPair` blockPos block)
     
left = moveBy (-1, 0)
right = moveBy (1, 0)
down = moveBy (0, -1)

blockRotate :: Double -> Block -> Block
blockRotate theta block =
    Block {
      blockPos = blockPos block,
      blockLocals = map (\x -> roundHalfPair (rotatePair x theta)) locals,
      blockKind = blockKind block
    }
    where locals = blockLocals block
          roundHalfPair p = (roundHalf $ fst p, roundHalf $ snd p)
          roundHalf x = 0.5 * (fromIntegral $ round $ 2.0 * x)

clockwise = blockRotate $ (-pi) / 2
    
addPair lhs rhs = (fst lhs + fst rhs, snd lhs + snd rhs)

-- >= doesn't do what i want
gePair lhs rhs = fst lhs >= fst rhs && snd lhs >= snd rhs 

-- < doesn't do what i want
ltPair lhs rhs = fst lhs < fst rhs && snd lhs < snd rhs

floorPair :: (Double, Double) -> (Int, Int)
floorPair p = (floor $ fst p, floor $ snd p)

rotatePair :: (Double, Double) -> Double -> (Double, Double)
rotatePair p theta =
    (c * fst p - s * snd p, s * fst p - c * snd p)
    where c = cos theta
          s = sin theta        

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe x True = Just x
boolToMaybe _ _    = Nothing

load block = mapBlockCells (\mp x -> Map.insert x (blockKind block) mp) block

unload block = mapBlockCells (\mp x -> Map.delete x mp) block

mapBlockCells :: (CellMap -> (Int, Int) -> CellMap) -> Block -> Board -> Board
mapBlockCells step block board =
    Board {
      boardSize = boardSize board,
      boardCells = foldl step (boardCells board) (blockCells block)
    }

mapBoardRows step board =
    Board {
      boardSize = boardSize board,
      boardCells = foldr step (boardCells board) [0 .. (boardHeight board) - 1]     
    }

inBound block board =
    all (\x -> x `gePair` origin
      && x `ltPair` boardSize board) (blockCells block)

collides block board =
    any (\x -> Map.member x (boardCells board)) (blockCells block)

loadBlockOrNot block nextBlock board gen =
    block `inBoundM` board     >>= \_ ->
    block `notCollidesM` board >>= \_ ->
    Just $ newGame block nextBlock (load block board) gen
    where notCollidesM b d = boolToMaybe b (not $ b `collides` d)
          inBoundM b d = boolToMaybe b $ b `inBound` d 

transform :: (Block -> Block) -> Game -> Maybe Game
transform f game =
    loadBlockOrNot block' nextBlock unloaded (gameGen game)
    where block' = f block
          block = gameBlock game
          nextBlock = gameNextBlock game
          unloaded = unload block (gameBoard game)

isFilled cells row width =
    all (\x -> Map.member (x, row) cells) [0 .. width - 1]

removeRow cells row =
    lower `Map.union` moved
    where removed = Map.filterWithKey (\ k _ -> snd k /= row) cells
          (lower, higher) = Map.partitionWithKey (\ k _ -> snd k < row) removed
          moved = Map.mapKeys (\k -> (fst k, snd k - 1)) higher

dropBlock var = maybe (tick var) dropBlock (transform down var)

tick :: Game -> Game 
tick game =
    fromMaybe hitTheFloor $ transform down game
    where hitTheFloor = fromMaybe game $ loadNewBlock game board'
          board' = mapBoardRows removeIfFilled board
          board = gameBoard game
          removeIfFilled row cells =
              if isFilled cells row (boardWidth board)
              then removeRow cells row
              else cells
          
loadNewBlock game board =
    loadBlockOrNot block' nextBlock' board gen'
    where block' = gameNextBlock game `moveTo` loadPos board
          nextBlock' = initBlock kind
          (kind, gen') = randomKind (gameGen game)

transformOrNot f game = fromMaybe game $ transform f game

transformOrNotG1M w f = transitionG1M w (transformOrNot f)

transitionG1M w f =
    transitionM w (\v -> newState (f $ varGame1 v) (varGame2 v))
    
-- state transition function needs to pass around w so it can be used to redraw.
transitionM :: C.Window -> (TetrixVar -> TetrixVar) -> TetrixState C.Window
transitionM w f =
    do var <- get 
       put (f var)
       return w

drawTetrix :: C.Window -> TetrixVar -> IO ()
drawTetrix w var = drawGame w origin (varGame1 var)

drawGame w base game =
    do drawCells w base h (Map.keys (boardCells board)) "x"
       drawCells w base h (blockCells block) "*"
       drawCells w base' h' (blockCells nextBlock) "x"
       C.mvWAddStr w (h + 2) 0 "press 'q' to quit."
    where board = gameBoard game
          block = gameBlock game
          nextBlock = gameNextBlock game 
          h = boardHeight board
          h' = nextBoardHeight
          base' = origin `addPair` (boardWidth board + 2, 0)
          
drawCells w base h cells s = do mapM_ (drawCell w base h s) cells
    
drawCell w base h s pos =
    C.mvWAddStr w (h - snd pos + snd base) (fst pos + fst base) s
    
redraw w =
   do liftIO $ C.wclear w
      var <- get
      liftIO $ drawTetrix w var
      liftIO $ C.wMove w 0 0
      liftIO $ C.refresh
      
eventloop :: C.Window -> Int -> TetrixState ()
eventloop w frame =
    do case frame of
           (x) | x == 0 -> process $ transitionG1M w tick
               | x > tickFrequency -> eventloop w 0
               | otherwise -> handleKeyEvent                     
    where process f =
            do w' <- f
               redraw w'
               eventloop w' $ frame + 1
          handleKeyEvent =
            do k <- liftIO C.getch
               case C.decodeKey k of
                 C.KeyChar 'q'  -> return ()
                 C.KeyChar ' '  -> process $ transitionG1M w dropBlock
                 C.KeyUp        -> process $ transformOrNotG1M w clockwise
                 C.KeyLeft      -> process $ transformOrNotG1M w left
                 C.KeyRight     -> process $ transformOrNotG1M w right
                 C.KeyDown      -> process $ transitionG1M w tick
                 _ -> eventloop w $ frame + 1

main :: IO ()
main =
    do runCurses `finally` C.endWin
    where runCurses =
            do w <- C.initScr
               seed <- (\x -> fst $ random x) `liftM` getStdGen
               C.keypad w True
               C.echo False
               C.cBreak True
               C.timeout defaultEventTimeoutMsec
               C.refresh
               runTetrix seed $ eventloop w 1
               