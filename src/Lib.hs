module Lib
    ( exampleFunc
    ) where

import Data.Tree

-- This is a simple tree with a single root (also a leaf)
-- and no subtrees
simpleTree = Node {
    rootLabel = "Hello, world!",
    subForest = []
}

exampleFunc :: IO ()
exampleFunc = putStrLn $ drawTree simpleTree
