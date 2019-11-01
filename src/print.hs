Module PRI where

  import System.Process


  keys public private = do
    let (e,n) = public
    system "clear"
    putStrLn "Your publick key is:"
    putStrLn $ "e: " ++ (show e) ++ "\n"
    putStrLn $ "n: " ++ (show n) ++ "\n"
    putStrLn "Your private key is:"
    putStrLn $ "d: " ++ (show private)  ++ "\n"
