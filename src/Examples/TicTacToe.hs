module Examples.TicTacToe where

import Data.List
import Data.Maybe
import Control.Monad

data TTTEntry = Circle | Cross | Empty
    deriving (Show, Eq)
type TTTLine  = [TTTEntry]
type TTTState = [TTTLine] -- List of rows

drawState :: TTTState -> String
drawState st = intercalate "\n" $ map lineDraw st
    where lineDraw line = intercalate "|" $ map entryDraw line
          entryDraw entry 
            | entry == Circle = "O"
            | entry == Cross  = "X"
            | otherwise       = " "

replace :: TTTEntry -> TTTState -> (Int, Int) -> TTTState
replace player st (rowIdx, colIdx) = do
    (currRowIdx, currRow) <- zip [0..] st
    if currRowIdx == rowIdx 
        then return $ replaceRow player currRow colIdx
        else return currRow
    where replaceRow player currRow colIdx = do
              (currColIdx, currEntry) <- zip [0..] currRow
              if currColIdx == colIdx
                  then return player
                  else return currEntry

nextStates :: TTTEntry -> TTTState -> [TTTState]
nextStates player st = do
    (rowNumber, row) <- zip [0..] st 
    (colNumber, entry) <- zip [0..] row
    guard (entry == Empty)
    return $ replace player st (rowNumber, colNumber)

initialState :: TTTState
initialState = take num $ repeat line 
    where line = take num $ repeat Empty
          num  = 3

getRow :: TTTState -> Int -> TTTLine
getRow st = (st !!)

getCol :: TTTState -> Int -> TTTLine
getCol st n = map (!! n) st

getForwardDiagonal :: TTTState -> TTTLine
getForwardDiagonal st = zipWith (!!) st [0..]

getBackwardsDiagonal :: TTTState -> TTTLine
getBackwardsDiagonal st = zipWith (!!) (reverse st) [0..]

isIdentical :: Eq a => [a] -> Bool
isIdentical line = and $ map ((head line) ==) $ tail line

emptyAsNothing :: TTTEntry -> Maybe TTTEntry
emptyAsNothing Empty = Nothing
emptyAsNothing other = Just other

winCondition :: TTTState -> Maybe TTTEntry
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

isFinished :: TTTState -> Bool
isFinished st = full st || (isJust $ winCondition st)
    where full st = not $ any (== Empty) $ join st
