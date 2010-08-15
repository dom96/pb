module Paste (newGist, newPastebin, pbExts, genGistData) where
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

genGistData :: String -> String -> String -> Bool -> String -> String -> [(String, String)]
genGistData contents filename ext priv usr token
  | (not $ null usr) && (not $ null token) =
    dat ++ [("login", usr), ("token", token)]
  | (not $ null usr) && (not $ null token) && priv =
    dat ++ [("login", usr), ("token", token)] ++ [("action_button", "private")] -- Gist currently doesn't support private gists through the API.
  | priv =
    dat ++ [("action_button", "private")]
  | otherwise = dat
  where dat = [("file_ext[gistfile1]", ext),
             ("file_name[gistfile1]", filename ++ ext),
             ("file_contents[gistfile1]", contents)]

newGist :: String -> String -> String -> Bool -> IO String
newGist contents filename ext private = do
    (usr, token) <- getGithubConfig
    (uri, rsp) <- browse $ do
                  setOutHandler (\a -> return ())
                  setErrHandler (\a -> return ())
                  request $ formToRequest $ Form POST gistUrl $
                      genGistData contents filename ext private usr token

    return $ show uri

-- Pastebin.com
pastebinUrl = fromJust $ parseURI "http://pastebin.com/api_public.php"

-- TODO: Add all of them.
pbExts = M.fromList
    [("txt", "text")
    ,("as", "actionscript3")
    ,("scpt", "applescript")
    ,("au3", "autoit")
    ,("avs", "avisynth")
    ,("sh", "bash")
    ,("bgl", "basic4gl")
    ,("bix", "bibtex")
    ,("bb", "blitzbasic")
    ,("c", "c")
    ,("cs", "csharp")
    ,("d", "d")
    ,("dcs", "dcs")
    ,("eff", "eiffel")
    ,("erl", "erlang")
    ,("for", "fortran")
    ,("hs", "haskell")
    ,("html", "html4strict")
    ,("java", "java5")
    ,("js", "javascript")
    ,("lsl", "lsl2")
    ,("s", "asm")
    ,("py", "python")]

genPastebinData :: String -> String -> Bool -> [(String, String)]
genPastebinData content syntax priv
  | priv = dat ++ [("paste_private", "1")]
  | otherwise = dat
  where dat = [("paste_code", content), ("paste_format", syntax)]

newPastebin :: String -> String -> Bool -> IO String
newPastebin contents syntax priv = do
    (uri, rsp) <- browse $ do
                  setOutHandler (\a -> return ())
                  setErrHandler (\a -> return ())
                  request $ formToRequest $ Form POST pastebinUrl $
                      genPastebinData contents syntax priv
    return $ rspBody rsp
