
module Build where

import Control.Monad
import qualified NewReleaseFiles as RF
import Render.Base
import Render.Linux
import Render.DownloadPage
import Render.PriorReleases
import Render.IncludedPackages
import HtmlTest
import System.Directory
import System.FilePath

{-

   TOP/
    - linux.html
    - windows.html
    - osx.html
    - 801/download.css
    - 801/download.js
    - 801/logo.png

-}

files801 :: [FileInfo]
files801 = concatMap RF._rls_files (RF.findReleaseNamed "8.0.1")

saveTo dir leaf content = do
  let path = dir </> leaf
  writeFile path content
  putStrLn $ "wrote " ++ path

copy src top path = do
  let dest = top </> path
  copyFile (src </> path) dest
  putStrLn $ "copied to " ++ dest

-- build all pages
buildAllPages src top = do
  createDirectoryIfMissing True top
  createDirectoryIfMissing True $ top </> "801"

  copy src top "801/download.css"
  copy src top "801/download.js"
  copy src top "801/logo.png"

  let files = files801
  saveTo top "linux.html"   $ blazeToString $ download_page_for_linux files
  saveTo top "windows.html" $ blazeToString $ download_page_for_windows files
  saveTo top "osx.html"     $ blazeToString $ download_page_for_osx files
  return ()

