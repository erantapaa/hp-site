module TestPage where

import Control.Monad
import qualified NewReleaseFiles as RF
import Render.Base
import Render.Linux
import Render.DownloadPage
import Render.PriorReleases
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


test3 = do
  let page = download_page files801
  writeFile "z.html" $ blazeToString page
  putStrLn "output written to z.html"

test4 = do
  let page = prior_releases_page RF.releaseFiles
  writeFile "y.html" $ blazeToString page
  putStrLn "output written to y.html"

