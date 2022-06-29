module SpecTreeTraversal
    ( main_treeTraversal )
    where

import TreeTraversal

import Data.Tree
import Test.HUnit
import System.Exit

testTree1 :: Tree Int
testTree1 = do
    (return 5) { subForest = [ leftSubTree, rightSubTree ] }
    where leftSubTree = (return 3) { subForest = [return 2, return 4] }
          rightSubTree = (return 7) { subForest = [return 6, return 8] }

testDFS1 = TestCase (assertEqual "Testing BFS" left right)
    where left  = dfs (== 3) testTree1
          right = Just 3

testBFS1 = TestCase (assertEqual "Testing DFS" left right)
    where left  = bfs (== 3) testTree1
          right = Just 3

main_treeTraversal :: IO Counts
main_treeTraversal  = do
    runTestTT ( test 
        [ testDFS1
        ])
