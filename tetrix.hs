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
  blockPos :: (Int, Int)
}

data Board = Board {
  boardSize :: (Int, Int),
  boardCells :: Map.Map (Int, Int) Cell
}

data TetrixVar = TetrixVar {
  gameBlock :: Block,
  gameBoard :: Board
}

type TetrixState = StateT TetrixVar IO

initBoard :: Int -> Int -> Board
initBoard width height =
    Board {
      boardSize = (width, height),
      boardCells = Map.empty
    }

initState :: TetrixVar
initState =
    TetrixVar {
      gameBlock = Block { blockPos = (5, 0) },
      gameBoard = initBoard defaultBoardWidth defaultBoardHeight
    }

runTetrix :: TetrixState a -> IO a
runTetrix m = evalStateT m initState

blockMoveBy :: (Int, Int) -> Block -> Block
blockMoveBy delta block =
    Block {
      blockPos = delta `addIntInt` blockPos block
    }
    
left = blockMoveBy (-1, 0)
right = blockMoveBy (1, 0)

addIntInt :: (Int, Int) -> (Int, Int) -> (Int, Int)
addIntInt lhs rhs = (fst lhs + fst rhs, snd lhs + snd rhs)

geIntInt :: (Int, Int) -> (Int, Int) -> Bool
geIntInt lhs rhs = fst lhs >= fst rhs && snd lhs >= snd rhs 

ltIntInt :: (Int, Int) -> (Int, Int) -> Bool
ltIntInt lhs rhs = fst lhs < fst rhs && snd lhs < snd rhs 

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe x b =
    if b
    then Just x
    else Nothing

load :: Block -> Board -> Board
load block board =
    Board {
      boardSize = boardSize board,
      boardCells = Map.insert (blockPos block) CTee $ boardCells board 
    }  

unload :: Block -> Board -> Board
unload block board =
    Board {
      boardSize = boardSize board,
      boardCells = Map.delete (blockPos block) $ boardCells board 
    }

inBound :: Block -> Board -> Maybe Block
inBound block board =
    boolToMaybe block $ (blockPos block) `geIntInt` origin
      && (blockPos block) `ltIntInt` (boardSize board)

collides :: Block -> Board -> Bool
collides block board =
    Map.member (blockPos block) (boardCells board)

transform :: (Block -> Block) -> TetrixVar -> Maybe TetrixVar
transform f var =
    (f block) `inBound` unloaded                  >>= \b' ->
    boolToMaybe b' (not $ b' `collides` unloaded) >>= \b' -> 
    Just TetrixVar {
      gameBlock = b',
      gameBoard = load block board
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
    do C.mvWAddStr w (snd pos) (fst pos) "*"
    where pos = blockPos $ gameBlock var

eventloop :: C.Window -> Int -> TetrixState ()
eventloop w tick =
    do case tick of
           (x) | x == 0 -> process $ transformM w $ blockMoveBy (0, 1)
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
               runTetrix $ eventloop w 0
               