module TreeTraversal where

import Data.Tree
import Data.Maybe
import Data.List
import Control.Applicative

dfs :: (a -> Bool) -> Tree a -> Maybe a
dfs test tree = do
    let root = rootLabel tree
    if not $ test root
        then do
            let sf = subForest tree
            listToMaybe $ mapMaybe (dfs test) sf
        else
            return root

toMaybe False _ = Nothing
toMaybe True x  = Just x

bfs :: (a -> Bool) -> Tree a -> Maybe a
bfs test tree = (toMaybe (test root) root) <|> (bfs' test $ subForest tree)
    where root = rootLabel tree
          bfs' :: (a -> Bool) -> [Tree a] -> Maybe a
          bfs' test sf = lineSearch <|> bfs' test (concat $ map subForest sf)
            where lineSearch = fmap rootLabel $ find (test . rootLabel) sf

