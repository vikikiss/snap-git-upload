{-# LANGUAGE OverloadedStrings #-}
module Web (site) where

import Job

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Data.List
import           Snap.Util.FileUploads
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS (toString)
import           Control.Monad.Trans
import           System.FilePath
import           Control.Monad
import           Data.Char
import qualified Data.ByteString.Base64 as Base64
import           Crypto.Hash.SHA1 (SHA1)
import           Crypto.HMAC (hmac', MacKey(..))
import qualified Crypto.Classes as Crypto (encode)
import           Control.Concurrent

site :: Chan Job -> Snap ()
site chan = foldr1 (<|>)
            [ ifTop (serveFile "static/main.html")
            , route [ ("login", serveFile "static/login.html")
                    , ("logout", logout)
                    , ("doLogin", login)
                    , ("upload", getUsername >> serveFile "static/upload.html")
                    , ("doUpload", getUsername >>= upload chan)
                    ]
            , dir "files" (serveDirectory "files/")
            ]

getUsername :: Snap ByteString
getUsername = forceJust maybeLoginCookie <|> redirect "/login"
  where
    maybeLoginCookie = do
        cookie <- forceJust $ getCookie "token"
        ipAddress <- getsRequest rqRemoteAddr
        return $ isValidLoginCookie ipAddress $ cookieValue cookie

upload :: Chan Job -> ByteString -> Snap ()
upload chan username = do
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

allowTxt :: PartInfo -> PartUploadPolicy
allowTxt pi = if "text/" `BS.isPrefixOf` (partContentType pi)
              then allowWithMaximumSize maxBound
              else disallow

login :: Snap ()
login = do
    username <- forceJust $ getPostParam "username"
    password <- forceJust $ getPostParam "password"
    guard $ username == "viki" && password == "foo"

    ipAddress <- getsRequest rqRemoteAddr
    modifyResponse $ addResponseCookie $ toLoginCookie username ipAddress
    redirect "/"

logout :: Snap ()
logout = do
    modifyResponse $ addResponseCookie noLoginCookie
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

noLoginCookie :: Cookie
noLoginCookie = Cookie
    { cookieName = "token"
    , cookieValue = BS.empty
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

