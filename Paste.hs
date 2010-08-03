module Paste (newGist) where
import Network.Browser
import Network.URI
import Network.HTTP
import Data.Maybe
import System.IO
import System.Process

getGithubConfig :: IO (String, String)
getGithubConfig = do
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "git config --global github.user"
    usr <- hGetLine pstdout
    (pstdin, pstdout, pstderr, ph) <- runInteractiveCommand $ "git config --global github.token"
    token <- hGetLine pstdout
    return (usr, token)

gistUrl = fromJust $ parseURI "http://gist.github.com/gists"

genPostData :: String -> String -> String -> String -> String -> [(String, String)]
genPostData contents filename ext usr token = 
    let dat = [("file_ext[gistfile1]", ext),
               ("file_name[gistfile1]", filename),
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
