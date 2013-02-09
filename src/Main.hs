{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import ProcessUpload
import Web

import Snap.Http.Server
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
    chan <- newChan
    forkIO $ do
        forever $ processUpload =<< readChan chan
    quickHttpServe $ site chan
