{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ProcessUpload
import Web

import Snap.Http.Server
import Control.Monad
import Control.Monad.Trans.Either
import Control.Concurrent

main :: IO ()
main = do
    chan <- newChan
    forkIO $ do
        forever $ do
            job <- readChan chan
            ex <- runEitherT $ processUpload job
            case ex of
                Left err -> print err
                Right () -> putStrLn "success!"
    quickHttpServe $ site chan
