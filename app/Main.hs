module Main where

import System.Random
import EGcd

main :: IO ()
main = do
  putStrLn "Wellcome!"
  putStrLn "Let's get started. How many bits do you want your key?"
  num_bits_in <-  getLine 
  let num_bits = (read num_bits_in :: Int)
  seed <- newStdGen
  putStrLn $ show $ eGCD 100 49
  putStrLn "end"
