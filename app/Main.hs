module Main where

import Compiler (compile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  (path, src) <- case args of
    [] -> do
      src <- getContents
      pure ("<stdin>", src)
    path : _ -> do
      src <- readFile path
      pure (path, src)
  case compile path src of
    Left err  -> hPutStrLn stderr $ show err
    Right bin -> putStr bin
