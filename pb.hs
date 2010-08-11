import System.IO
import System.Environment
import System.Process
import System.Console.GetOpt
import Data.Maybe
import qualified Data.Map as M
import Paste
    
getClipboard :: IO String
getClipboard = do
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "xclip -o"
    eof <- hIsEOF pstdout
    if not $ eof
        then hGetContents pstdout
        else return ""

pastebinCB :: String -> String -> String -> IO ()
pastebinCB name ext pb = do
    -- Get the contents of the clipboard
    contents <- getClipboard
    if not $ null contents
        then case pb of "gist"    -> do url <- newGist contents name ('.':ext)
                                        putStrLn url
                        "pb"      -> do url <- newPastebin contents 
                                          (fromJust (M.lookup ext pbExts)) 
                                        putStrLn url
                        -- Leaving this here, just incase.
                        otherwise -> putStrLn "Invalid pastebin provider" 
                        
        else putStrLn "Error: Nothing in the clipboard"

pastebin :: String -> String -> String -> IO ()
pastebin name ext pb = do
    putStrLn $ "About to pastebin " ++ name ++ ('.':ext)
    putStrLn "Paste the code you want to pastebin and press CTRL + D to pastebin."
    contents <- getContents

    case pb of "gist"    -> do url <- newGist contents name ('.':ext)
                               putStrLn url
               "pb"      -> do url <- newPastebin contents 
                                  (fromJust (M.lookup ext pbExts))
                               putStrLn url
               otherwise -> putStrLn "Invalid pastebin provider"

data Options = Options
 {  
   optClipboard   :: Bool
 , optShowVersion :: Bool
 , optShowHelp    :: Bool
 , optPasteName   :: String
 , optPasteExt    :: String
 , optPastebin    :: String -- Pastebin provider('gist' or 'pb')
 } deriving Show

defaultOptions    = Options
 { optClipboard   = False
 , optShowVersion = False
 , optShowHelp    = False
 , optPasteName   = "file"
 , optPasteExt    = "txt"
 , optPastebin    = "gist" 
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
        (ReqArg (\f opts -> opts { optPastebin = f }) "gist | pb" ) "Select a Pastebin provider"
    ]
header = "Usage: pb [options]"

pbOpts :: [String] -> IO (Options, [String])
pbOpts args =
    case getOpt RequireOrder options args of 
        (xs, n, []) -> return (foldl (flip id) defaultOptions xs, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

parseOpts :: Options -> IO ()
parseOpts xs
    | optShowVersion xs = putStrLn "pb 0.1"
    | optShowHelp xs    = putStrLn $ usageInfo header options
    | optClipboard xs   = pastebinCB (optPasteName xs) (optPasteExt xs) (optPastebin xs)
    | isInvalidExt xs   = putStrLn "Invalid file extension"
    | isInvalidPb xs    = putStrLn "Invalid pastebin provider"
    | otherwise         = pastebin (optPasteName xs) (optPasteExt xs) (optPastebin xs)
    -- TODO: Rewrite this in free point style. Should look nicer then.
    where isInvalidExt xs = (not $ isJust $ M.lookup (optPasteExt xs) pbExts) &&
                             optPastebin xs /= "gist"
          isInvalidPb xs  = not (optPastebin xs == "gist" || optPastebin xs == "pb")

main = do
    args <- getArgs
    opts <- pbOpts args

    case opts of
        (xs, _)  -> parseOpts xs

