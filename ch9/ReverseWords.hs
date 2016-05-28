main = do
    line <- getLine
    if null line
        then putStrLn "buh bye!"
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
