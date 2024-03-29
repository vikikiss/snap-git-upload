{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module ProcessUpload (processUpload) where

import Job

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BS (toString)
import qualified Data.ByteString.Char8 as BS (putStrLn)

import Control.Monad (unless, void)
import Control.Monad.Trans.Either
import Control.Monad.Trans
import HSH.Command
import System.FilePath
import System.Exit
import Data.Monoid

processUpload :: Job -> IO ()
processUpload job = do
    result <- runEitherT $ do
        moveToDestination job
        compile job
        git job
    cleanup
    case result of
        Left err -> do
            putStrLn "Error!"
            BS.putStrLn err
        Right () -> putStrLn "success!"

repoDir :: FilePath
repoDir = "files/repo"

runShell :: String -> [String] -> EitherT ByteString IO ()
runShell cmd args = do
    (out, err, force) <- liftIO $ run (cmd, args)
    ((_ :: String), exitCode) <- liftIO force
    unless (exitCode == ExitSuccess) $
      left $ out <> "\n" <> err

moveToDestination :: Job -> EitherT ByteString IO ()
moveToDestination job = runShell "mv" ["-f", filePath job, repoDir </> "src" </> fileName job]

compile :: Job -> EitherT ByteString IO ()
compile job = runShell "javac" ["-d", repoDir </> "classes", repoDir </> "src" </> fileName job]

runGit :: [String] -> EitherT ByteString IO ()
runGit args = runShell "git" $
                  [ "--work-tree", repoDir
                  , "--git-dir", repoDir </> ".git"
                  ] ++ args

cleanup :: IO ()
cleanup = void $ runEitherT (runGit ["reset", "--hard"])

git :: Job -> EitherT ByteString IO ()
git job = do
    runGit ["add", "src" </> fileName job]
    runGit ["commit"
           , "-m", "commit message goes here"
           , "--author", author
           ]
    runGit ["push", "origin", "HEAD"]
  where
    author = unwords $ map BS.toString $ [username job, email]
    email = "<" <> username job <> "@example.com" <> ">"
