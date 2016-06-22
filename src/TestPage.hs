module TestPage where

import Control.Monad
import qualified NewReleaseFiles as RF
import Render.Base
import Render.Linux
import Render.DownloadPage
import Render.PriorReleases
import Render.IncludedPackages
import HtmlTest

files801 :: [FileInfo] 
files801 = concatMap RF._rls_files (RF.findReleaseNamed "8.0.1")

linuxBins files = binsFor RF.OsLinux files
linuxSrc files = srcDists files

test1 = do
  let section = linux_download (linuxBins files801) (linuxSrc files801)
  putStrLn $ blazeToPrettyString section

test2 = do
  let got = canonicalizeBlaze $ linux_download (linuxBins files801) (linuxSrc files801)
  expected <- fmap canonicalizeTrees $ readHTML "/tmp/linux.html"
  runDiff expected got

writeToFile path content = do
  writeFile path content
  putStrLn $ "output written to " ++ path

test3 = do
  let page = download_page files801
  writeToFile "z.html" $ blazeToString page

test6 = do
  -- generate the linux, osx and windows pages
  let files = files801
  writeToFile "linux.html" $ blazeToString $ download_page_for_linux files
  writeToFile "windows.html" $ blazeToString $ download_page_for_windows files
  writeToFile "osx.html" $ blazeToString $ download_page_for_osx files

