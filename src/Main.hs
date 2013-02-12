{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ProcessUpload
import Web

import Snap.Http.Server
import Control.Monad
import Control.Monad.Trans.Either
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    chan <- newChan
    forkIO $ do
        forever $ do
            job <- readChan chan
            ex <- runEitherT $ processUpload job
            case ex of
                Left err -> do
                    putStrLn "Error!"
                    BS.putStrLn err
                Right () -> putStrLn "success!"
    quickHttpServe $ site chan
