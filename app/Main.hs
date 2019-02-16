module Main where

import           Compiler (compile)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath (dropExtension)
import qualified System.IO as IO

main :: IO ()
main = do
  args <- getArgs
  (path, src, out) <- case args of
    [] -> do
      src <- IO.getContents
      pure ("<stdin>", src, IO.stdout)
    path : _ -> do
      src <- IO.readFile path
      out <- IO.openFile (dropExtension path) IO.WriteMode
      pure (path, src, out)
  case compile path src of
    Left err  -> do
      IO.hPutStrLn IO.stderr $ show err
      exitFailure
    Right bin -> do
      IO.hPutStr out bin
      IO.hClose out
