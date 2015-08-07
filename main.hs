module Main where
import LCParser

main :: IO ()
main = putStrLn . programToString . lcparseString $ "x = [\\z.z q];y = x;q = [\\r.y];"

 
 
