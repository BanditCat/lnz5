module Main where
import LCParser
import Lamping
import Diagrams.Backend.SVG.CmdLine


main :: IO ()
main = do putStrLn $ graphString g
          mainWith $ diagramGraph g
  where g = makeGraph (lcparseString "x = \\r q f t y.q q q q q;yrly = \\z.z x x;prob = [[\\g.[g [g [\\x.x]]]][\\h.[[\\f.[f [f [\\z.z]]]][h [\\y.y]]]]];inc7 = [\\x.[\\y. [[\\f.[[\\h.[h [\\p.[h[\\q.q]]]]]][\\l.[[[f [\\n. [l n]]] x] y]]]][\\g. [\\u. \\v.[[g u] [g v]]]]]];") "inc7"
