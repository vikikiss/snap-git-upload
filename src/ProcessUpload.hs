{-# LANGUAGE OverloadedStrings #-}
module ProcessUpload (processUpload) where

import Job
import Data.ByteString
import Control.Monad.Trans.Either
import Control.Monad.Trans

processUpload :: Job -> EitherT ByteString IO ()
processUpload job = do
    lift $ print job

compile :: Job -> EitherT ByteString IO ()
compile job = undefined

git :: Job -> EitherT ByteString IO ()
git job = undefined
