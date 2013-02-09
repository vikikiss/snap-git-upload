module Job where

import Data.ByteString

data Job = Job { username :: ByteString
               , filePath :: FilePath
               }
           deriving Show

