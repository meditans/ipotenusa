module Main where

import Lib

main :: IO ()
main = writeFile "/tmp/ipotenuse.txt" . show . triplesLessThan $ 10^7
