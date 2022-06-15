import Data.Tree

-- This is a simple tree with a single root (also a leaf)
-- and no subtrees
simpleTree = Node {
    rootLabel = "Hello, world!",
    subForest = []
}

main = do putStrLn "Hello, world!"
