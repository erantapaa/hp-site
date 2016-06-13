{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Prior Releases page

module Render.PriorReleases where

import Control.Monad
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html4.Strict.Attributes as A

import Render.Base
import Render.HaskellOrg (branding_style, hl_head, hl_footer, navbar_section, banner_left)

import Data.List

import qualified NewReleaseFiles as RF

renderFileInfo :: Int -> FileInfo -> String
renderFileInfo year fileinfo
  | year >= 2016 = "???"
  | otherwise    = RF.distName (RF._disttype fileinfo)

renderRelease :: Release -> Html
renderRelease rls = do
  let monthName = RF.monthName (RF._rls_month rls)
      version = RF._rls_name rls
      year = RF._year (RF._rls_month rls)
      files = RF._rls_files rls

  p $ do strong (toMarkup version)
         ", " >> toMarkup monthName >> " " >> toMarkup year
         sequence_
           $ intersperse " - "
           $ [ a ! href (stringValue (RF._url file)) $ toMarkup (renderFileInfo year file) | file <- files ]

prior_releases_page :: [Release] -> Html
prior_releases_page releases = do
  let groups = RF.groupBySameYear releases
  H.head $ do
    hl_head
    H.title "Prior Releases"
    branding_style
    -- XXX hp_head
  body ! class_ "page-home" $ do
    H.div ! class_ "wrap" $ do
      navbar_section
      -- the banner area
      H.div ! class_ "pattern-bg" $ do
        H.div ! class_ "container" $ do
          H.div ! class_ "row" $ do
            H.div ! class_ "span12 col-sm-12" $ do
              banner_left
           -- H.div ! class_ "span6 col-sm-6" $ do
           -- banner_right
      -- main content
      H.div ! class_ "container" $ do
        h2 $ "Prior Releases"
        forM_ groups $ \rels -> do
          let year =  RF._year (RF._rls_month (Prelude.head rels))
          h3 ! A.id "section" $ toMarkup year
          forM_ rels $ \rel -> do renderRelease rel
    hl_footer

