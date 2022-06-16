import Data.List

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
