{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Data.String
import           Data.List
import           Snap.Util.FileUploads
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BS (toString)
import           Control.Monad.Trans
import Data.Monoid
import Data.Maybe
import System.FilePath
import System.Directory
import Control.Monad

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = foldr1 (<|>)
       [ ifTop (serveFile "static/main.html")
       , route [ ("process", needLoginCookie $ processHandler)
               , ("login", serveFile "static/login.html")
               , ("loginprocess", loginProcessHandler)
               , ("listingFiles", listFilesHandler)
               ]
       , needLoginCookie $ dir "upload" (serveFile "static/upload.html")
       , dir "files" (serveDirectory "files/")
       ]

needLoginCookie :: Snap a -> Snap a
needLoginCookie m = do
    -- TODO: check cookie
    cookie <- getCookie "foo"
    case cookie of
        Just c -> return ()
        Nothing -> redirect "/login"
    hasCookie <- return True
    if hasCookie then m else redirect "/login"

extForContentType :: ByteString -> Maybe FilePath
extForContentType "image/jpeg" = Just ".jpg"
extForContentType "image/png" = Just ".png"
extForContentType _ = Nothing

allowImages :: PartInfo -> PartUploadPolicy
allowImages pi = if isJust (extForContentType (partContentType pi))
                 then allowWithMaximumSize maxBound
                 else disallow

processHandler :: Snap ()
processHandler = do
    handleFileUploads "files" defaultUploadPolicy allowImages $ \parts -> do
        case find (\(pi, _) -> partFieldName pi == "file") parts of
            Just (pi, Right tmpFileName) -> do
                let ext = fromMaybe ".dat" $ extForContentType (partContentType pi)
                bs <- liftIO $ BS.readFile tmpFileName
                fileName <- getParam "fileName"
                fileName' <- case fileName of
                    Nothing -> do
                        writeText "<p>Error</p>"
                        modifyResponse $ setResponseStatus 500 "Internal Server Error"
                        finishWith =<< getResponse
                    Just fn -> return $ if BS.null fn then
                                          BS.toString $ fromJust (partFileName pi)
                                        else
                                          takeBaseName (BS.toString fn) <.> ext
                liftIO $ BS.writeFile ("files" </> fileName') bs

                modifyResponse (setContentType "text/html")
                writeText $ uploadedMessage fileName' (BS.length bs)
            _ -> writeBS $ fromString $ show parts

uploadedMessage :: FilePath -> Int -> Text
uploadedMessage fileName fileSize =
    T.unlines [ "<p>You have uploaded a file</p>"
              , "<p>" <> T.unwords ["Size:", fromString $ show fileSize, "B"] <> "</p>"
              , "<a href='http://google.com/'>link</a>"
              ]

listFilesHandler :: Snap ()
listFilesHandler = do
    filePaths <- liftIO $ getDirectoryContents "files"
    let fps = filter (\fp -> fp /= "." && fp /= "..") $ filePaths
    forM_ (sort fps) (\filePath -> do
                                 writeText ("<p><a href='files/" `mappend` fromString filePath `mappend` "'>" `mappend` fromString filePath `mappend` "</a></p>"))
    -- writeBS $ fromString filePath)

loginProcessHandler :: Snap ()
loginProcessHandler = do
    username <- getPostParam "username"
    password <- getPostParam "password"
    case username of
        Nothing -> return ()
        Just username' -> case password of
            Nothing -> return ()
            Just password' -> do
                when (username' == "viki" && password' == "foo") $ do
                  let cookie = Cookie{ cookieName="foo"
                                     , cookieValue="titok"
                                     , cookieExpires=Nothing
                                     , cookieDomain=Nothing
                                     , cookiePath=Nothing
                                     , cookieSecure=False
                                     , cookieHttpOnly=True }
                  modifyResponse $ addResponseCookie cookie
                  redirect "/"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
