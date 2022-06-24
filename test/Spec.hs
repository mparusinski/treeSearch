import Test.HUnit
import System.Exit

import Examples.SpecTicTacToe

main :: IO ()
main = do
    putStrLn "### Testing Examples.TicTacToe ###"
    ticTacToeCounts <- main_ticTacToe
    if (errors ticTacToeCounts + failures ticTacToeCounts == 0)
        then exitSuccess
        else exitFailure
