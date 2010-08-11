module Paste (newGist, newPastebin, pbExts) where
import Network.Browser
import Network.URI
import Network.HTTP
import Data.Maybe
import System.IO
import System.Process
import qualified Data.Map as M

-- Gist
getUserGithub :: IO String
getUserGithub = do
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "git config --global github.user"
    eof <- hIsEOF pstdout
    if not $ eof
        then hGetLine pstdout
        else return ""

getTokenGithub :: IO String
getTokenGithub = do
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "git config --global github.token"
    eof <- hIsEOF pstdout
    if not $ eof
        then hGetLine pstdout
        else return ""

getGithubConfig :: IO (String, String)
getGithubConfig = do
    usr <- getUserGithub
    token <- getTokenGithub
    return (usr, token)

gistUrl = fromJust $ parseURI "http://gist.github.com/gists"

genGistData :: String -> String -> String -> String -> String -> [(String, String)]
genGistData contents filename ext usr token = 
    let dat = [("file_ext[gistfile1]", ext),
               ("file_name[gistfile1]", filename ++ ext),
               ("file_contents[gistfile1]", contents)] in
    if (not $ null usr) && (not $ null token)
        then dat ++ [("login", usr), ("token", token)]
        else dat

newGist :: String -> String -> String -> IO String
newGist contents filename ext = do
    (usr, token) <- getGithubConfig
    (uri, rsp) <- browse $ do
                  setOutHandler (\a -> return ())
                  setErrHandler (\a -> return ())
                  request $ formToRequest $ Form POST gistUrl $
                      genGistData contents filename ext usr token

    return $ show uri

-- Pastebin.com
pastebinUrl = fromJust $ parseURI "http://pastebin.com/api_public.php"

-- TODO: Add all of them.
pbExts = M.fromList
    [("txt", "text")
    ,("php", "php")
    ,("cpp", "cpp")
    ,("hs", "haskell")
    ,("c", "c")
    ,("h", "c")
    ,("hpp", "cpp")
    ,("s", "asm")
    ,("b", "bf")
    ,("sh", "bash")
    ,("asp", "asp")
    ,("html", "html4strict")
    ,("tpl", "php")
    ,("ini", "ini")
    ,("lisp", "lisp")
    ,("cs", "csharp")
    ,("py", "python")]

genPastebinData :: String -> String -> [(String, String)]
genPastebinData content syntax = 
    [("paste_code", content), ("paste_format", syntax)]

newPastebin :: String -> String -> IO String
newPastebin contents syntax = do
    (uri, rsp) <- browse $ do
                  setOutHandler (\a -> return ())
                  setErrHandler (\a -> return ())
                  request $ formToRequest $ Form POST pastebinUrl $
                      genPastebinData contents syntax
    return $ rspBody rsp
