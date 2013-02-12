module Job where

import Data.ByteString

data Job = Job { username :: ByteString
               , filePath :: FilePath -- ^ Temporary file name in files/queue (e.g. "files/queue/upload1234.java")
               , fileName :: FilePath -- ^ Original file name (e.g. "MyClass.java")
               }
           deriving Show

