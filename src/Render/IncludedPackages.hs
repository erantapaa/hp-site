
{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Prior Releases page

module Render.IncludedPackages where

import Control.Monad
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html4.Strict.Attributes as A

import Render.Base
import Render.HaskellOrg (hp_stylesheet, hl_head, hl_footer, navbar_section, banner_with_links, HPMenuItem(..))

import Data.List
import HtmlDoc

import qualified NewReleaseFiles as RF

included_packages_page contents_body doc =
  let d1 = appendHead doc $ do
                hl_head
                H.title "Included Packages"
                hp_stylesheet
                include_jquery
                script ! src (asset "contents.js") $ mempty
      d2 = appendBody d1 $ do
              H.div ! class_ "wrap" $ do
                navbar_section
                -- the banner area
                H.div ! class_ "pattern-bg" $ do
                  H.div ! class_ "container" $ do
                    H.div ! class_ "row" $ do
                      H.div ! class_ "span12 col-sm-12" $ do
                        banner_with_links Contents
                     -- H.div ! class_ "span6 col-sm-6" $ do
                     -- banner_right
                -- main content
                preEscapedString contents_body
              hl_footer
  in d2 

