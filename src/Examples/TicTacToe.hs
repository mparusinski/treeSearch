module Examples.TicTacToe where

import Data.List
import Data.Maybe
import Data.Tree
import Control.Monad
import Control.Monad.State.Lazy

data TTTEntry = Circle | Cross | Empty
    deriving (Show, Eq)
type TTTPlayer = TTTEntry
type TTTLine  = [TTTEntry]
type TTTBoard = [TTTLine] -- List of rows

treeBuilderInitialState :: State TTTPlayer (Tree TTTBoard)
treeBuilderInitialState = do
    return Node {
        rootLabel = initialBoard,
        subForest = []
    }

alternatePlayer :: TTTPlayer -> TTTPlayer
alternatePlayer Circle = Cross
alternatePlayer Cross  = Circle
alternatePlayer _      = error "Player is either Circle or Cross"

treeBuilderExpand :: Tree TTTBoard -> State TTTPlayer (Tree TTTBoard)
treeBuilderExpand tree = do
    currentPlayer <- get
    put (alternatePlayer currentPlayer)
    return $ expandTreeH currentPlayer tree
    where expandTreeH cp tree 
            | null $ subForest tree = tree { subForest = map return (nb cp tree) }
            | otherwise             = tree { subForest = map (expandTreeH cp) $ subForest tree }
            where nb cp tree1 = nextBoards cp $ rootLabel tree1 

-- This function is very slow as it builds the entire tree
treeBuilderFullTree :: State TTTPlayer (Tree TTTBoard)
treeBuilderFullTree = do
    initialTree <- treeBuilderInitialState
    iterateBuild initialTree
    where iterateBuild currTree
            | isComplete currTree = return currTree
            | otherwise           = do
                nextTree <- treeBuilderExpand currTree
                iterateBuild nextTree

isComplete :: Tree TTTBoard -> Bool
isComplete tree
    | null $ subForest tree = isFinished $ rootLabel tree
    | otherwise             = and (map isComplete $ subForest tree)

drawBoard :: TTTBoard -> String
drawBoard st = intercalate "\n" $ map lineDraw st
    where lineDraw line = intercalate "|" $ map entryDraw line
          entryDraw entry 
            | entry == Circle = "O"
            | entry == Cross  = "X"
            | otherwise       = " "

updateBoard :: TTTEntry -> TTTBoard -> (Int, Int) -> TTTBoard
updateBoard player st (rowIdx, colIdx) = do
    (currRowIdx, currRow) <- zip [0..] st
    if currRowIdx == rowIdx 
        then return $ updateRow player currRow colIdx
        else return currRow
    where updateRow player currRow colIdx = do
              (currColIdx, currEntry) <- zip [0..] currRow
              if currColIdx == colIdx
                  then return player
                  else return currEntry

nextBoards :: TTTEntry -> TTTBoard -> [TTTBoard]
nextBoards player st = do
    (rowNumber, row) <- zip [0..] st 
    (colNumber, entry) <- zip [0..] row
    guard (entry == Empty)
    return $ updateBoard player st (rowNumber, colNumber)

initialBoard :: TTTBoard
initialBoard = take num $ repeat line 
    where line = take num $ repeat Empty
          num  = 3

getRow :: TTTBoard -> Int -> TTTLine
getRow st = (st !!)

getCol :: TTTBoard -> Int -> TTTLine
getCol st n = map (!! n) st

getForwardDiagonal :: TTTBoard -> TTTLine
getForwardDiagonal st = zipWith (!!) st [0..]

getBackwardsDiagonal :: TTTBoard -> TTTLine
getBackwardsDiagonal st = zipWith (!!) (reverse st) [0..]

isIdentical :: Eq a => [a] -> Bool
isIdentical line = and $ map ((head line) ==) $ tail line

emptyAsNothing :: TTTEntry -> Maybe TTTEntry
emptyAsNothing Empty = Nothing
emptyAsNothing other = Just other

winCondition :: TTTBoard -> Maybe TTTEntry
winCondition st = join $ fmap emptyAsNothing $ msum [rowWin, colWin, fdWin, bdWin]
    where rowWin = fmap (head . (rows !!)) $ findIndex isIdentical rows
          colWin = fmap (head . (cols !!)) $ findIndex isIdentical cols
          fdWin  = if isIdentical fdiag then Just $ head fdiag else Nothing
          bdWin  = if isIdentical bdiag then Just $ head bdiag else Nothing
          fdiag  = getForwardDiagonal st
          bdiag  = getBackwardsDiagonal st
          rows   = map (getRow st) [0..tl]
          cols   = map (getCol st) [0..tl]
          tl     = length st - 1

isFinished :: TTTBoard -> Bool
isFinished st = full st || (isJust $ winCondition st)
    where full st = not $ any (== Empty) $ join st
