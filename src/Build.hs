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
import qualified Text.Blaze.Html5 as H
import Data.Monoid
import HtmlDoc

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

-- The list of downloads availble for this release.
availableDownloads :: [FileInfo]
availableDownloads = concatMap RF._rls_files (RF.findReleaseNamed "8.0.1")

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

-- build all pages placing files into top
buildAllPages :: FilePath -> IO ()
buildAllPages top = do
  createDirectoryIfMissing True top
  createDirectoryIfMissing True $ top </> "801"

  -- Download Page
  let assetdir= top </> asset_dir
  copyDataFile assetdir "download.css"
  copyDataFile assetdir "download.js"
  copyDataFile assetdir "logo.png"
  copyDataFile assetdir "contents.js"

  analytics <- getDataFileName "analytics.script" >>= T.readFile

  -- convert Html to T.Text and inject analytics
  let buildPage = injectAtEnd analytics . blazeToText
      buildDoc  d = renderText $ d `prependEnd` (H.preEscapedText analytics)

  let files = availableDownloads
  saveTo top "linux.html"   $ buildDoc $ download_page_for_linux' emptyDoc files
  saveTo top "windows.html" $ buildDoc $ download_page_for_windows' emptyDoc files
  saveTo top "osx.html"     $ buildDoc $ download_page_for_osx' emptyDoc files

  copyFile (top </> "windows.html") (top </> "index.html")

  -- Prior Releases Page
  let doc = (prior_releases_page emptyDoc (tail RF.allReleases))
  saveTo top "prior.html" $ buildDoc doc

  -- Included Packages Page
  contents_body <- getDataFileName "contents-body.html" >>= readFile
  let doc = included_packages_page contents_body emptyDoc
  saveTo top "contents.html" $ buildDoc doc

  return ()

