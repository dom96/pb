import System.IO
import System.Environment
import System.Process
import System.Console.GetOpt
import Data.Maybe
import Paste

getClipboard :: IO String
getClipboard = do
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "xclip -o"
    eof <- hIsEOF pstdout
    if not $ eof
        then hGetContents pstdout
        else return ""

pastebinCB name ext = do
        -- Get the contents of the clipboard
	contents <- getClipboard
        if not $ null contents
            then do url <- newGist contents name ext
                    putStrLn $ show url
            else putStrLn "Error: Nothing in the clipboard"

pastebin name ext = do
    putStrLn $ "About to pastebin " ++ name ++ ext
    putStrLn "Paste the code you want to pastebin and press CTRL + D to pastebin."
    contents <- getContents

    url <- newGist contents name ext
    putStrLn $ show url


data Options = Options
 {
   optPasteSyntax :: String
 , optPasteBin    :: Bool
 , optClipboard   :: Bool
 , optShowVersion :: Bool
 , optPasteName   :: String
 , optPasteExt    :: String
 , optShowHelp    :: Bool
 } deriving Show

defaultOptions    = Options
 { optClipboard   = False
 , optShowVersion = False
 , optShowHelp    = False
 , optPasteName   = "file"
 , optPasteExt    = ".txt"
 , optPasteBin    = False
 , optPasteSyntax = "text"
 }


options :: [OptDescr (Options -> Options)] 
options = 
    [ Option ['v'] ["version"] 
        (NoArg (\opts -> opts { optShowVersion = True })) "show version number"
    , Option ['c'] ["clipboard"] 
        (NoArg (\opts -> opts { optClipboard = True })) "pastebin from clipboard"
    , Option ['n'] ["name"] 
        (ReqArg (\f opts -> opts { optPasteName = f })
            "paste name")
        "pastebin with the specified name"
    , Option ['e'] ["ext"] 
        (ReqArg (\f opts -> opts { optPasteExt = f })
            "paste extension")
        "pastebin with the extension name"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optShowHelp = True })) "help"
    , Option ['p'] ["pastebin"]
        (NoArg (\opts -> opts { optPasteBin = True })) "Paste to pastebin.com"
    , Option ['s'] ["syntax"]
        (ReqArg (\f opts -> opts { optPasteSyntax = f }) 
            "Set syntax when pasting to pastebin.com")
    ]
header = "Usage: pb [options]"

pbOpts args =
    case getOpt RequireOrder options args of 
        (xs, n, []) -> return (foldl (flip id) defaultOptions xs, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

parseOpts xs
    | optShowVersion xs = do putStrLn "pb 0.1"
    | optShowHelp xs    = do putStrLn $ usageInfo header options
    | optClipboard xs   = do pastebinCB (optPasteName xs) (optPasteExt xs)
    | optPasteBin xs    = newPasteBin (optPasteSyntax xs)
    | otherwise         = do pastebin   (optPasteName xs) (optPasteExt xs)
main = do
    args <- getArgs
    opts <- pbOpts args

    case opts of
        (xs, _)  -> parseOpts xs
    --putStrLn $ "Got args " ++ (show args)
    --parseArgs args
