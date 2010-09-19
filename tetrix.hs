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
seed = 0 :: Int

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

data TetrixVar = TetrixVar {
  gameBlock :: Block,
  gameBoard :: Board,
  gameGen :: StdGen
}

type TetrixState = StateT TetrixVar IO

runTetrix :: TetrixState a -> IO a
runTetrix m = evalStateT m initState

boardWidth board = fst (boardSize board)

boardHeight board = snd (boardSize board)

initBoard width height =
    Board {
      boardSize = (width, height),
      boardCells = Map.empty
    }
    
initBlock board kind =
    Block {
      blockPos = ((boardWidth board) `div` 2, (boardHeight board) - 2),
      blockLocals = initLocals kind,
      blockKind = kind
    }

initLocals kind =
    case kind of
      Tee -> [(0.0, 0.0), (-1.0, 0.0), (1.0, 0.0), (0.0, 1.0)]
      Bar -> [(0.0, -1.5), (0.0, -0.5), (0.0, 0.5), (0.0, 1.5)]
      Box -> [(-0.5, 0.5), (0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)]
      El  -> [(0.0, 0.0), (0.0, 1.0), (0.0, -1.0), (1.0, -1.0)]
      Jay -> [(0.0, 0.0), (0.0, 1.0), (0.0, -1.0), (-1.0, -1.0)]
      Es  -> [(-0.5, 0.0), (0.5, 0.0), (-0.5, 1.0), (0.5, -1.0)]
      Zee -> [(-0.5, 0.0), (0.5, 0.0), (-0.5, -1.0), (0.5, 1.0)]

initState =
    TetrixVar {
      gameBlock = block,
      gameBoard = board,
      gameGen = gen'
    }
    where board = initBoard defaultBoardWidth defaultBoardHeight
          (kind, gen') = randomKind (mkStdGen seed)
          block = initBlock board kind

randomKind gen =
    (head $ drop value cells, gen')
    where (value, gen') = randomR (0, length cells - 1) gen
          cells = [Tee, Bar, Box, El, Jay, Es, Zee]
          
randomKindTransition var =
    (kind,
    TetrixVar {
      gameBlock = gameBlock var,
      gameBoard = gameBoard var,
      gameGen = gen'
    })
    where (kind, gen') = randomKind (gameGen var)

blockCells :: Block -> [(Int, Int)]
blockCells block =
    map (\x -> floorPair x `addPair` blockPos block) $ blockLocals block

blockMoveBy :: (Int, Int) -> Block -> Block
blockMoveBy delta block =
    Block {
      blockPos = delta `addPair` blockPos block,
      blockLocals = blockLocals block,
      blockKind = blockKind block
    }
    
left = blockMoveBy (-1, 0)
right = blockMoveBy (1, 0)
down = blockMoveBy (0, -1)

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
boolToMaybe x b =
    if b
    then Just x
    else Nothing

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

loadBlockOrNot block board var =
    block `inBoundM` board     >>= \_ ->
    block `notCollidesM` board >>= \_ ->
    Just TetrixVar {
      gameBlock = block,
      gameBoard = load block board,
      gameGen = gameGen var
    }
    where notCollidesM b d = boolToMaybe b (not $ b `collides` d)
          inBoundM b d = boolToMaybe b $ b `inBound` d 

transform :: (Block -> Block) -> TetrixVar -> Maybe TetrixVar
transform f var =
    loadBlockOrNot block' unloaded var
    where block' = f block
          block = gameBlock var
          unloaded = unload block (gameBoard var)

isFilled cells row width =
    all (\x -> Map.member (x, row) cells) [0 .. width - 1]

removeRow cells row =
    lower `Map.union` moved
    where removed = Map.filterWithKey (\ k _ -> snd k /= row) cells
          (lower, higher) = Map.partitionWithKey (\ k _ -> snd k < row) removed
          moved = Map.mapKeys (\k -> (fst k, snd k - 1)) higher
    
tick var =
    fromMaybe hitTheFloor $ transform down var
    where hitTheFloor = fromMaybe var $ loadNewBlock var board'
          board' = mapBoardRows removeIfFilled board
          board = gameBoard var
          existing = boardCells board
          removeIfFilled row cells =
              if isFilled cells row (boardWidth board)
              then removeRow cells row
              else cells
          
loadNewBlock var board =
    loadBlockOrNot block' board var'
    where block' = initBlock board kind
          (kind, var') = randomKindTransition var

transformOrNot f var = fromMaybe var $ transform f var

transformOrNotM w f = transitionM w (transformOrNot f)

-- state transition function needs to pass around w so it can be used to redraw.
transitionM :: C.Window -> (TetrixVar-> TetrixVar) -> TetrixState C.Window
transitionM w f =
    do var <- get
       let var' = f var
       put var'
       return w

drawTetrix :: C.Window -> TetrixVar -> IO ()
drawTetrix w var =
    do drawBoard w board
    where board = gameBoard var
          pos = blockPos $ gameBlock var
    
drawBoard w board =
    do mapM_ drawCell cells
    where cells = Map.keys (boardCells board)
          drawCell pos = C.mvWAddStr w ((boardHeight board) - snd pos) (fst pos) "*"
    
redraw w =
   do liftIO $ C.wclear w
      var <- get
      liftIO $ drawTetrix w var
      liftIO $ C.wMove w 0 0
      liftIO $ C.refresh
      
eventloop :: C.Window -> Int -> TetrixState ()
eventloop w frame =
    do case frame of
           (x) | x == 0 -> process $ transitionM w tick
               | x > 7 -> eventloop w 0
               | otherwise -> handleKeyEvent                     
    where process f =
            do w' <- f
               redraw w'
               eventloop w' $ frame + 1
          handleKeyEvent =
            do k <- liftIO C.getch
               case C.decodeKey k of
                 C.KeyChar 'q'  -> return ()
                 C.KeyUp        -> process $ transformOrNotM w clockwise
                 C.KeyLeft      -> process $ transformOrNotM w left
                 C.KeyRight     -> process $ transformOrNotM w right
                 C.KeyDown      -> process $ transitionM w tick
                 _ -> eventloop w $ frame + 1

main :: IO ()
main =
    do runCurses `finally` C.endWin
    where runCurses =
            do w <- C.initScr
               C.keypad w True
               C.echo False
               C.cBreak True
               C.timeout 100
               C.refresh
               runTetrix $ eventloop w 1
               