module Main where

import           Data.ByteString    (readFile, writeFile)
import           HollowKnightSaveED
import           Prelude            hiding (readFile, writeFile)
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    case args of
      ["-e", fileName] -> (readFile fileName) >>= (writeFile ("encrypted"++fileName) . encryptSave)
      ["-d", fileName] -> (readFile fileName) >>= (mapM_ (writeFile ("decrypted"++fileName)) . decryptSave)
      _    -> putStrLn ("usage: " ++ progName ++ " -[e/d] fileName")
