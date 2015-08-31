module Main where
import LCParser
import Lamping
import Rules
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size


main :: IO ()
main = do fc <- lcparseFile "base.lnz"
          let lg = makeGraph fc "inc2" in
            do putStrLn $ graphString lg
               putStrLn $ expToString $ buildProgram lg
               putStrLn $ getLineString fc "inc2"
               putStrLn $ show (graphList lg)
               
               renderSVG "lnz.svg" (dims2D 1920 1080) (diagramRules lg
                         [
                          (rule1a, 0),
                          (rule1a, 0),
                          (rule3a, 0),
                          (rule2a, 0),
                          (rule4b, 0),
                          (rule4d, 0),
                          (rule4d, 0),
                          (rule7h, 0),
                          (rule7h, 0),
                          (rule7f, 0),
                          (rule6e, 0),
                          (rule2a, 0),
                          (rule7c, 0),
                          (rule7c, 0),
                          (rule7c, 0),
                          (rule2a, 0),
                          (rule7c, 0)
                         ] 128 6)
