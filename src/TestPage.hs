
module TestPage where

import Control.Monad
import ReleaseFiles as RF
import Render.Base
import Render.Linux
import Render.DownloadPage
import Render.PriorReleases
import HtmlTest

files801 = findRelease "8.0.1"

findRelease name = 
  case matches of
    ((_,_,fs):_) -> fs
    _            -> error $ "unable to find release named " ++ name
  where
    matches = filter go RF.releaseFiles
                where go (s,_,_) = s == name

linuxBins files = [ f | f <- files, rf_isBinFor OsLinux f ]
linuxSrc files = [ f  | f <- files, rf_isSource f ]

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
  let page = prior_releases_page releaseFiles
  writeFile "y.html" $ blazeToString page
  putStrLn "output written to y.html"

