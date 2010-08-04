import System.IO
import System.Environment
import System.Process
import Paste

getClipboard :: IO String
getClipboard = do
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "xclip -o"
    eof <- hIsEOF pstdout
    if not $ eof
        then hGetContents pstdout
        else return ""

parseArgs :: [String] -> IO ()
parseArgs (x:xs)
    | x == "-c" || x == "--clipboard" = do
        -- Get the contents of the clipboard
        contents <- getClipboard
        if not $ null contents
            then do url <- newGist contents "test" ".txt"
                    putStrLn $ show url
            else putStrLn "Error: Nothing in the clipboard"
        
    | otherwise = do
        putStrLn "Usage: pb [options]\n\
                 \pb --help          This help message\n\
                 \pb -c[--clipboard] Pastebin whatever is in the clipboard"
parseArgs [] = do
    putStrLn "Paste the code you want to pastebin and press CTRL + D to pastebin."
    contents <- getContents

    url <- newGist contents "test" ".txt"
    putStrLn $ show url

main = do
    args <- getArgs
    putStrLn $ "Got args " ++ (show args)
    parseArgs args

