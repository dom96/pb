import System.IO
import System.Environment
import Paste

parseArgs :: [String] -> IO ()
parseArgs ["--help"] = do
    putStrLn "Usage: pb [options]\n\
             \pb --help          This help message"
parseArgs [] = do
    putStrLn "Paste the code you want to pastebin and press CTRL + D to pastebin."
    contents <- getContents

    url <- newGist contents "test" ".txt"
    putStrLn $ show url

main = do
    args <- getArgs
    putStrLn $ "Got args " ++ (show args)
    parseArgs args

