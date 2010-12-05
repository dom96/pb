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

safeLookup a
  | isInvalidExt a = a
  | otherwise = fromJust $ M.lookup a pbExts
  where isInvalidExt xs = (not $ isJust $ M.lookup xs pbExts)

getStdin :: Options -> IO String
getStdin (Options _ _ _ _ _ name ext pbName _) = do
  putStrLn $ "About to pastebin " ++ name ++ ('.':ext) ++ " to " ++ pbName
  putStrLn "Paste the code you want to pastebin and press CTRL + D to pastebin."
  getContents

pastebin :: Options -> IO ()
pastebin opts = do
  contents <- getContents
  
  if not $ null contents
    then do case (optPastebin opts) of 
              "gist"    -> do url <- newGist $ newInfo opts contents
                              putStrLn url
              "pb"      -> do url <- newPastebin $ newInfo opts contents
                              putStrLn url
              otherwise -> putStrLn "Invalid pastebin provider"
    else reportError
  where clipboard   = optClipboard opts
        getContents = if clipboard
                        then getClipboard
                        else getStdin opts
        reportError = if clipboard
                        then putStrLn "Error: Nothing in the clipboard."
                        else putStrLn "Error: Got nothing from stdin."
        
newInfo :: Options -> String -> PastebinInfo
newInfo (Options _ _ _ priv expires name ext _ expireDate) contents = 
  PastebinInfo contents name ext priv expires expireDate

data Options = Options
 {  
   optClipboard   :: Bool
 , optShowVersion :: Bool
 , optShowHelp    :: Bool
 , optPrivate     :: Bool
 , optExpires     :: Bool
 , optPasteName   :: String
 , optPasteExt    :: String
 , optPastebin    :: String -- Pastebin provider('gist' or 'pb')
 , optExpirationDate :: String
 } deriving Show

defaultOptions    = Options
 { optClipboard   = False
 , optShowVersion = False
 , optShowHelp    = False
 , optPrivate     = False
 , optExpires     = False
 , optPasteName   = "file"
 , optPasteExt    = "txt"
 , optPastebin    = "gist"
 , optExpirationDate = "1H"
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
    , Option ['x'] ["ext"] 
        (ReqArg (\f opts -> opts { optPasteExt = f })
            "paste extension")
        "pastebin with the extension name"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optShowHelp = True })) "help"
    , Option ['b'] ["pastebin"]
        (ReqArg (\f opts -> opts { optPastebin = f }) "gist | pb" ) "Select a Pastebin provider."
    , Option ['p'] ["priv"] 
        (NoArg (\opts -> opts { optPrivate = True })) "Make this paste private."
    , Option ['e'] ["expire"] 
        (NoArg (\opts -> opts { optExpires = True })) "Make this paste expire, default is 1 Hour."
    , Option ['d'] ["expire-date"]
        (ReqArg (\f opts -> opts { optExpirationDate = f }) 
            "date") 
        "Specifies the expiration date"
    ]
header = "Usage: pb [options]"

pbOpts :: [String] -> IO (Options, [String])
pbOpts args =
    case getOpt RequireOrder options args of 
        (xs, n, []) -> return (foldl (flip id) defaultOptions xs, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

parseOpts :: Options -> IO ()
parseOpts opts
    | optShowVersion opts = putStrLn "pb 0.1.1"
    | optShowHelp opts    = putStrLn $ usageInfo header options
    | isInvalidPb opts    = putStrLn "Invalid pastebin provider"
    | optClipboard opts   = pastebin opts
    | otherwise           = pastebin opts
    
    -- TODO: Rewrite this in free point style. Should look nicer then.
    where isInvalidPb xs  = not (optPastebin xs == "gist" || optPastebin xs == "pb")

main = do
    args <- getArgs
    opts <- pbOpts args

    case opts of
        (xs, _)  -> parseOpts xs

