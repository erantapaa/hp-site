{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as BlazeT
import Data.Monoid

import Paths_hp_site

-- blazeToText :: Html -> T.Text
blazeToText = TL.toStrict . BlazeT.renderHtml

injectAtEnd :: T.Text -> T.Text -> T.Text
injectAtEnd scripts html = insertBefore "</body>" scripts html

-- insert new before target in text
insertBefore :: T.Text -> T.Text -> T.Text -> T.Text
insertBefore target new text =
  let (match, post) = T.breakOnEnd target text
      pre = T.take (T.length match - T.length target) match
  in 
  if not (T.isSuffixOf target match)
    then let fragment = T.unpack $ T.drop (T.length text - 50) text
         in error $ "insertBefore - could not find: " <> T.unpack target <> " in: " <> fragment
    else pre <> new <> target <> post


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

saveTo :: FilePath -> String -> T.Text -> IO ()
saveTo dir leaf content = do
  let path = dir </> leaf
  T.writeFile path content
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
  let assetdir= top </> "801"
  copyDataFile assetdir "download.css"
  copyDataFile assetdir "download.js"
  copyDataFile assetdir "logo.png"

  analytics <- getDataFileName "analytics.script" >>= T.readFile

  -- convert Html to T.Text and inject analytics
  let buildPage = injectAtEnd analytics . blazeToText

  let files = files801
  saveTo top "linux.html"   $ buildPage $ download_page_for_linux files
  saveTo top "windows.html" $ buildPage $ download_page_for_windows files
  saveTo top "osx.html"     $ buildPage $ download_page_for_osx files

  copyFile (top </> "windows.html") (top </> "index.html")

  -- Prior Releases
  let page = prior_releases_page (tail RF.allReleases)
  saveTo top "prior.html"  $ buildPage page

  -- Included Packages
  contents_body <- getDataFileName "contents-body.html" >>= readFile
  let page = included_packages_page contents_body
  saveTo top "contents.html" $ buildPage page

  return ()

