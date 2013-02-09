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
import Data.Char
import qualified Data.ByteString.Base64 as Base64
import Crypto.Hash.SHA1 (SHA1)
import Crypto.HMAC (hmac', MacKey(..))
import qualified Crypto.Classes as Crypto (encode)
import Control.Concurrent

data Job = Job { username :: ByteString
               , filePath :: FilePath
               }
           deriving Show

main :: IO ()
main = do
    chan <- newChan
    forkIO $ do
        forever $ do
            result <- readChan chan
            print result
    quickHttpServe $ site chan

site :: Chan Job -> Snap ()
site chan = foldr1 (<|>)
            [ ifTop (serveFile "static/main.html")
            , route [ ("process", processHandler)
                    , ("login", serveFile "static/login.html")
                    , ("loginprocess", loginProcessHandler)
                    , ("listingFiles", listFilesHandler)
                    , ("fileUpload", serveFile "static/fileUpload.html")
                    , ("txtfileprocess", getUsername >>= txtFileHandler chan)
                    ]
            , dir "upload" $ getUsername >> serveFile "static/upload.html"
            , dir "files" (serveDirectory "files/")
            ]

getUsername :: Snap ByteString
getUsername = forceJust maybeLoginCookie <|> redirect "/login"
  where
    maybeLoginCookie = do
        cookie <- forceJust $ getCookie "token"
        ipAddress <- getsRequest rqRemoteAddr
        return $ isValidLoginCookie ipAddress $ cookieValue cookie

txtFileHandler :: Chan Job -> ByteString -> Snap ()
txtFileHandler chan username = do
    handleFileUploads "files/txtFiles" defaultUploadPolicy allowTxt $ \parts -> do
        case find (\(pi, _) -> partFieldName pi == "file") parts of
            Just (pi, Right tmpFileName) -> do
                bs <- liftIO $ BS.readFile tmpFileName
                fileName <- case partFileName pi of
                    Nothing -> do
                        writeText "<p>Error</p>"
                        modifyResponse $ setResponseStatus 500 "Internal Server Error"
                        finishWith =<< getResponse
                    Just fn -> return $ takeFileName (BS.toString fn)
                let fileName' = "files/txtFiles" </> fileName
                liftIO $ BS.writeFile fileName' bs

                let job = Job {username = username
                              , filePath = fileName'}
                liftIO $ writeChan chan job

                modifyResponse (setContentType "text/html")
                writeText "Upload was successful"
            _ -> writeBS $ fromString $ show parts

forceJust :: Snap (Maybe a) -> Snap a
forceJust m = do
    mx <- m
    case mx of
        Just x -> return x
        Nothing -> pass

extForContentType :: ByteString -> Maybe FilePath
extForContentType "image/jpeg" = Just ".jpg"
extForContentType "image/png" = Just ".png"
extForContentType _ = Nothing

allowImages :: PartInfo -> PartUploadPolicy
allowImages pi = if isJust (extForContentType (partContentType pi))
                 then allowWithMaximumSize maxBound
                 else disallow

allowTxt :: PartInfo -> PartUploadPolicy
allowTxt pi = if "text/" `BS.isPrefixOf` (partContentType pi)
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
        Just username -> case password of
            Nothing -> return ()
            Just password -> do
                when (username == "viki" && password == "foo") $ do
                    ipAddress <- getsRequest rqRemoteAddr
                    modifyResponse $ addResponseCookie $ toLoginCookie username ipAddress
                    redirect "/"

toLoginCookie :: ByteString -> ByteString -> Cookie
toLoginCookie username ipAddress = Cookie
    { cookieName = "token"
    , cookieValue = BS.intercalate ":" [username, ipAddress, toToken username ipAddress]
    , cookieExpires = Nothing
    , cookieDomain = Nothing
    , cookiePath = Nothing
    , cookieSecure = False
    , cookieHttpOnly = True
    }

isValidLoginCookie :: ByteString -> ByteString -> Maybe ByteString
isValidLoginCookie ipAddressReq cookie = case BS.split (fromIntegral $ ord ':') cookie of
    [username, ipAddress, token] -> do
        guard $ ipAddressReq == ipAddress && toToken username ipAddress == token
        Just username
    _ -> Nothing

toToken :: ByteString -> ByteString -> ByteString
toToken username ipAddress = encode . hash $ BS.intercalate ":" [username, ipAddress]
  where
    hash :: ByteString -> SHA1
    hash = hmac' (MacKey secret)

    encode :: SHA1 -> ByteString
    encode = Base64.encode . Crypto.encode

    secret :: ByteString
    secret = "ae90e6abc7db9fe32f97674e4c0849cf"

