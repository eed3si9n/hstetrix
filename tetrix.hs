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
    
initBlock board =
    Block {
      blockPos = (fst s `div` 2, snd s - 2),
      blockLocals = [(0.0, 0.0), (-1.0, 0.0), (1.0, 0.0), (0.0, 1.0)]
    }
    where s = boardSize board

initState =
    TetrixVar {
      gameBlock = initBlock board,
      gameBoard = board
    }
    where board = initBoard defaultBoardWidth defaultBoardHeight

blockCells :: Block -> [(Int, Int)]
blockCells block =
    map (\x -> roundPair x `addPair` blockPos block) $ blockLocals block

blockMoveBy :: (Int, Int) -> Block -> Block
blockMoveBy delta block =
    Block {
      blockPos = delta `addPair` blockPos block,
      blockLocals = blockLocals block
    }
    
left = blockMoveBy (-1, 0)
right = blockMoveBy (1, 0)
down = blockMoveBy (0, -1)

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

collides block board =
    any (\x -> Map.member x $ boardCells board) $ blockCells block

loadBlockOrNot block board var =
    block `inBoundM` board     >>= \_ ->
    block `notCollidesM` board >>= \_ ->
    Just TetrixVar {
      gameBlock = block,
      gameBoard = load block board
    }
    where notCollidesM b d = boolToMaybe b (not $ b `collides` d)
          inBoundM b d = boolToMaybe b $ b `inBound` d 

transform :: (Block -> Block) -> TetrixVar -> Maybe TetrixVar
transform f var =
    loadBlockOrNot block' unloaded var
    where block' = f block
          block = gameBlock var
          unloaded = unload block $ gameBoard var

tick var =
    fromMaybe hitTheFloor $ transform down var
    where hitTheFloor = fromMaybe var $ loadNewBlock var
    
loadNewBlock var =
    loadBlockOrNot block' board var
    where block' = initBlock board
          board = gameBoard var

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
    do mapM_ (\pos ->
         C.mvWAddStr w (defaultBoardHeight - snd pos) (fst pos) "*") $ Map.keys $ boardCells board

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
               