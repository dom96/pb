module Paste (newGist, newPasteBin) where
import Network.Browser
import Network.URI
import Network.HTTP
import Data.Maybe
import System.IO
import System.Process

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

genPostData :: String -> String -> String -> String -> String -> [(String, String)]
genPostData contents filename ext usr token = 
    let dat = [("file_ext[gistfile1]", ext),
               ("file_name[gistfile1]", filename ++ ext),
               ("file_contents[gistfile1]", contents)] in
    if (not $ null usr) && (not $ null token)
        then dat ++ [("login", usr), ("token", token)]
        else dat

newGist :: String -> String -> String -> IO URI
newGist contents filename ext = do
    (usr, token) <- getGithubConfig
    (uri, rsp) <- browse $ do
                  setOutHandler (\a -> return ())
                  setErrHandler (\a -> return ())
                  request $ formToRequest $ Form POST gistUrl $
                      genPostData contents filename ext usr token

    return uri


pastebinUrl = fromJust $ parseURI "http://pastebin.com/api_public.php"

genPasteBinData :: String -> String -> [(String, String)]
genPasteBinData content syntax = 
	 [("paste_code", content),
	 ("paste_format", syntax)]

newPasteBin :: String -> String -> IO URI
newPasteBin syntax contents = do
    (uri, rsp) <- browse $ do 
        request $ formToRequest $ Form POST pastebinUrl $
            genPasteBinData contents syntax
    return uri
