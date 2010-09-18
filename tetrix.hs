module Main where

import qualified Data.Map as Map
import Data.Maybe
import Control.Exception
import Control.Monad.State
import qualified UI.HSCurses.Curses as C

defaultBoardWidth = 9
defaultBoardHeight = 20
origin = (0, 0)

data Cell = CTee

data Block = Block {
  blockPos :: (Int, Int),
  blockLocals :: [(Double, Double)]
}

type CellMap = Map.Map (Int, Int) Cell

data Board = Board {
  boardSize :: (Int, Int),
  boardCells :: CellMap
}

data TetrixVar = TetrixVar {
  gameBlock :: Block,
  gameBoard :: Board
}

type TetrixState = StateT TetrixVar IO

runTetrix :: TetrixState a -> IO a
runTetrix m = evalStateT m initState

initBoard width height =
    Board {
      boardSize = (width, height),
      boardCells = Map.empty
    }
    
initBlock =
    Block {
      blockPos = (defaultBoardWidth `div` 2, defaultBoardHeight - 3),
      blockLocals = [(0.0, 0.0), (-1.0, 0.0), (1.0, 0.0), (0.0, 1.0)]
    }

initState =
    TetrixVar {
      gameBlock = initBlock,
      gameBoard = initBoard defaultBoardWidth defaultBoardHeight
    }

blockCells :: Block -> [(Int, Int)]
blockCells block = map (\x -> roundPair x `addPair` blockPos block) $ blockLocals block

blockMoveBy :: (Int, Int) -> Block -> Block
blockMoveBy delta block =
    Block {
      blockPos = delta `addPair` blockPos block,
      blockLocals = blockLocals block
    }
    
left = blockMoveBy (-1, 0)
right = blockMoveBy (1, 0)

addPair lhs rhs = (fst lhs + fst rhs, snd lhs + snd rhs)

gePair lhs rhs = fst lhs >= fst rhs && snd lhs >= snd rhs 

ltPair lhs rhs = fst lhs < fst rhs && snd lhs < snd rhs

roundPair :: (Double, Double) -> (Int, Int)
roundPair p = (round $ fst p, round $ snd p)

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe x b =
    if b
    then Just x
    else Nothing

load block =
    mapBlockCells step block
    where step mp x = Map.insert x CTee mp

unload block =
    mapBlockCells step block
    where step mp x = Map.delete x mp

mapBlockCells :: (CellMap -> (Int, Int) -> CellMap) -> Block -> Board -> Board
mapBlockCells step block board =
    Board {
      boardSize = boardSize board,
      boardCells = foldl step existing $ blockCells block
    }
    where existing = boardCells board

inBound block board =
    all (\x -> x `gePair` origin
      && x `ltPair` boardSize board) $ blockCells block

inBoundM block board = boolToMaybe block $ block `inBound` board 

collides block board =
    any (\x -> Map.member x $ boardCells board) $ blockCells block

transform :: (Block -> Block) -> TetrixVar -> Maybe TetrixVar
transform f var =
    (f block) `inBoundM` unloaded                 >>= \b' ->
    boolToMaybe b' (not $ b' `collides` unloaded) >>= \b' -> 
    Just TetrixVar {
      gameBlock = b',
      gameBoard = load b' unloaded
    }
    where block = gameBlock var
          board = gameBoard var
          unloaded = unload block board

transformOrNot :: (Block -> Block) -> TetrixVar -> TetrixVar
transformOrNot f var = fromMaybe var $ transform f var
       
transformM :: C.Window -> (Block -> Block) -> TetrixState C.Window
transformM w f =
    do var <- get
       let var' = transformOrNot f var
       put var'
       return w

redraw :: C.Window -> Int -> TetrixState ()
redraw w tick =
    do liftIO $ C.wclear w
       var <- get
       liftIO $ drawTetrix w var
       liftIO $ C.wMove w 0 0
       liftIO $ C.refresh

drawTetrix :: C.Window -> TetrixVar -> IO ()
drawTetrix w var =
    do drawBoard w board
    where board = gameBoard var
          pos = blockPos $ gameBlock var
    
drawBoard w board =
    do mapM_ (\pos ->
         C.mvWAddStr w (defaultBoardHeight - snd pos) (fst pos) "*") $ Map.keys $ boardCells board

eventloop :: C.Window -> Int -> TetrixState ()
eventloop w tick =
    do case tick of
           (x) | x == 0 -> process $ transformM w $ blockMoveBy (0, -1)
               | x > 10 -> eventloop w 0
               | otherwise -> handleKeyEvent                     
    where process f =
            do w' <- f
               redraw w' tick
               eventloop w' $ tick + 1
          handleKeyEvent =
            do k <- liftIO C.getch
               case C.decodeKey k of
                 C.KeyChar 'q'  -> return ()
                 C.KeyLeft      -> process $ transformM w left
                 C.KeyRight     -> process $ transformM w right
                 _ -> eventloop w $ tick + 1

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
               