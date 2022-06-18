import Examples.TicTacToe

import Test.HUnit

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

testInitialState = TestCase (assertEqual left right)
    where left  = initialState
          right = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

testGetRow1 = TestCase (assertEqual left right)
    where left  = getRow testState1 3
          right = [Circle, Cross, Empty] 
 
testGetRow2 = TestCase (assertEqual left right)
    where left  = getRow testState2 1
          right = [Cross, Cross, Circle]

testGetCol1 = TestCase (assertEqual left right)
    where left  = getCol testState3 2
          right = [Circle, Cross, Cross]

testGetCol1 = TestCase (assertEqual left right)
    where left  = getCol testState4 3
          right = [Cross, Cross, Circle]

