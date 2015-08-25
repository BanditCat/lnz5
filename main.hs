module Main where
import LCParser
import Lamping
import Diagrams.Backend.SVG.CmdLine
import Data.Set as Set


main :: IO ()
main = do fc <- lcparseFile "base.lnz"
          let lg = makeGraph fc "inc2" in
            do putStrLn $ graphString lg
               putStrLn $ expToString $ buildProgram lg
               putStrLn $ getLineString fc "inc2"
               mainWith (diagramRules lg
                         [(rule1a, 0),
                          (rule1a, 0)
                         ] 500)
