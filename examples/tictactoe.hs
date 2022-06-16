import Data.List
import Control.Monad

data TTTEntry = Circle | Cross | Empty
    deriving (Show, Eq)
type TTTLine  = [TTTEntry]
type TTTState = [TTTLine] -- List of rows

examplettt = [
    [Circle, Circle, Cross],
    [Empty, Cross, Empty],
    [Circle, Cross, Empty]]

drawState :: TTTState -> String
drawState st = intercalate "\n" $ map lineDraw st
    where lineDraw line = intercalate "|" $ map entryDraw line
          entryDraw entry 
            | entry == Circle = "O"
            | entry == Cross  = "X"
            | otherwise       = " "

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

winCondition :: TTTState -> Maybe TTTEntry
winCondition st = msum [rowWin, colWin, fdWin, bdWin]
    where rowWin = fmap (head . (rows !!)) $ findIndex isIdentical rows
          colWin = fmap (head . (cols !!)) $ findIndex isIdentical cols
          fdWin  = if isIdentical fdiag then Just $ head fdiag else Nothing
          bdWin  = if isIdentical bdiag then Just $ head bdiag else Nothing
          fdiag  = getForwardDiagonal st
          bdiag  = getBackwardsDiagonal st
          rows   = map (getRow st) [0..tl]
          cols   = map (getCol st) [0..tl]
          tl     = length st - 1
