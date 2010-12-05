{-# LANGUAGE OverloadedStrings #-}
module Paste 
  ( PastebinInfo(..)
  , newGist
  , newPastebin
  , pbExts
  , genGistData
  ) where
import Network.Browser
import Network.URI
import Network.HTTP
import qualified Network.HTTP.Enumerator as HTTPE
import Data.Maybe
import System.IO
import System.Process
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

data PastebinInfo = PastebinInfo
  { pbContents   :: String
  , pbFilename   :: String
  , pbExt        :: String
  , pbPriv       :: Bool
  , pbExpires    :: Bool
  , pbExpireDate :: String
  }
  
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

gistUrl = "https://gist.github.com/gists"

genGistData :: PastebinInfo -> B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)]
genGistData pbInfo usr token =
  let dat = [("file_ext[gistfile1]", B.pack $ pbExt pbInfo),
             ("file_name[gistfile1]", B.pack $ pbFilename pbInfo ++ ('.':pbExt pbInfo)),
             ("file_contents[gistfile1]", B.pack $ pbContents pbInfo)]
  in dat ++ user ++ private
  where user = 
          if (not $ B.null usr) && (not $ B.null token)
            then [("login", usr), ("token", token)]
            else []
        private =
          if pbPriv pbInfo
            then [("action_button", "private")]
            else []

newGist :: PastebinInfo -> IO String
newGist pbInfo = HTTPE.withHttpEnumerator $ do
  (usr, token) <- getGithubConfig
  req <- HTTPE.parseUrl gistUrl
  let req2 = HTTPE.urlEncodedBody (genGistData pbInfo (B.pack usr) (B.pack token)) req
  rsp <- HTTPE.httpLbs req2
  let url = lookup "Location" (HTTPE.responseHeaders rsp)
  if isJust url
    then return $ B.unpack $ fromJust url
    else return $ "Error creating gist: Unable to find the Location header."

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

getSyntax :: String -> String
getSyntax ext
  | isJust syntax = fromJust syntax
  | otherwise     = ext
  where syntax = M.lookup ext pbExts

genPastebinData pbInfo = 
  let dat = [("paste_code", pbContents pbInfo), 
             ("paste_format", getSyntax $ pbExt pbInfo)]
  in dat ++ private ++ expires
  where private = 
          if pbPriv pbInfo 
            then [("paste_private", "1")] 
            else []
        expires = 
          if pbExpires pbInfo
            then [("paste_expires", "1"), 
                  ("paste_expire_date", pbExpireDate pbInfo)]
            else []

newPastebin :: PastebinInfo -> IO String
newPastebin pbInfo = do
    (uri, rsp) <- browse $ do
                  setOutHandler (\a -> return ())
                  setErrHandler (\a -> return ())
                  request $ formToRequest $ Form POST pastebinUrl $
                      genPastebinData pbInfo
    return $ rspBody rsp
