module Examples.SpecTicTacToe
    ( main_ticTacToe )
    where

import Examples.TicTacToe

import Test.HUnit
import System.Exit

-- Unfinished state (non winning)
testState1 = [
    [Circle, Circle, Cross],
    [Empty, Cross, Empty],
    [Circle, Cross, Empty]]

-- Finished state (non winning)
testState2 = [
    [Circle, Circle, Cross],
    [Cross, Cross, Circle],
    [Circle, Cross, Cross]]

-- Finished state (X winning)
testState3 = [
    [Circle, Circle, Cross],
    [Cross, Cross, Cross],
    [Circle, Cross, Circle]]

-- Finished state (O winning)
testState4 = [
    [Circle, Cross, Cross],
    [Cross, Circle, Cross],
    [Cross, Cross, Circle]]

testInitialState = TestCase (assertEqual "Initial state" left right)
    where left  = initialState
          right = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

testGetRow1 = TestCase (assertEqual "Get row #1" left right)
    where left  = getRow testState1 2
          right = [Circle, Cross, Empty] 
 
testGetRow2 = TestCase (assertEqual "Get row #1" left right)
    where left  = getRow testState2 0
          right = [Cross, Cross, Circle]

testGetCol1 = TestCase (assertEqual "Get col #1" left right)
    where left  = getCol testState3 1
          right = [Circle, Cross, Cross]

testGetCol2 = TestCase (assertEqual "Get col #2" left right)
    where left  = getCol testState4 2
          right = [Cross, Cross, Circle]

main_ticTacToe :: IO Counts
main_ticTacToe  = do
    runTestTT ( test [
        testInitialState,
        testGetRow1
        ])