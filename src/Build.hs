
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

import Paths_hp_site

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

-- copy a cabal data file
copyDataFile top path = do
  src <- getDataFileName path
  let dest = top </> path
  copyFile src dest
  putStrLn $ "copied to " ++ dest

-- build all pages
buildAllPages top = do
  createDirectoryIfMissing True top
  createDirectoryIfMissing True $ top </> "801"

  -- Download Page
  copyDataFile top "download.css"
  copyDataFile top "download.js"
  copyDataFile top "logo.png"

  let files = files801
  saveTo top "linux.html"   $ blazeToString $ download_page_for_linux files
  saveTo top "windows.html" $ blazeToString $ download_page_for_windows files
  saveTo top "osx.html"     $ blazeToString $ download_page_for_osx files

  copyFile (top </> "windows.html") (top </> "index.html")

  -- Prior Releases
  let page = prior_releases_page (tail RF.allReleases)
  saveTo top "prior.html"  $ blazeToString page

  -- Included Packages
  contents_body <- getDataFileName "contents-body.html" >>= readFile
  let page = included_packages_page contents_body
  saveTo top "contents.html" $ blazeToString page

  return ()

