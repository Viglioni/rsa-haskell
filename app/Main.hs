module Main where

import System.Random
import System.Process
import RSAkeys

main :: IO ()
main = do
  system "clear"
  putStrLn "Wellcome!"
  putStrLn "Let's get started. How many bits do you want your key?"
  num_bits_in <-  getLine 
  let num_bits = (read num_bits_in :: Int)
  seed1 <- newStdGen
  seed2 <- newStdGen
  let ((e,n),d) = RSAkeys.gen_keys_by_bits num_bits seed1 seed2
  system "clear"
  putStrLn "Your publick key is:"
  putStrLn $ "e: " ++ (show e)
  putStrLn $ "n: " ++ (show n)
  putStrLn "Your private key is:"
  putStrLn $ "d: " ++ (show d)
  putStrLn "end"
