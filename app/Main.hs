module Main where

import System.Environment
import System.Exit
import Build (buildAllPages)

usage = do
  die "Usage: hp-site INSTALL-DIRECTORY"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> buildAllPages dir
    _     -> usage


