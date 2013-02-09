{-# LANGUAGE OverloadedStrings #-}
module ProcessUpload (processUpload) where

import Job

processUpload :: Job -> IO ()
processUpload job = do
    print job
