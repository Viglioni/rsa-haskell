module Print where

import System.Process

keys public private = do
  let (e,n) = public
  system "clear"
  putStrLn "Your publick key is:"
  putStrLn $ "e: " ++ (show e) ++ "\n"
  putStrLn $ "n: " ++ (show n) ++ "\n"
  putStrLn "Your private key is:"
  putStrLn $ "d: " ++ (show private)  ++ "\n"

welcome = do
  system "clear"
  putStrLn "Wellcome!"
  putStrLn "Let's get started. How many bits do you want your key?"

whatDoYouWannaDo = do
  system "clear"
  putStrLn "What do you want to do?"
  putStrLn "1. show keys"
