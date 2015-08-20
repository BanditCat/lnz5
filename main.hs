module Main where
import LCParser
import Lamping
import Diagrams.Backend.SVG.CmdLine
import Data.Set as Set


main :: IO ()
main = do fc <- lcparseFile "base.lnz"
       	  putStrLn $ graphString (makeGraph fc "inc7")
          mainWith $ diagramGraph (makeGraph fc "inc7") (Set.fromList [])
