
{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Prior Releases page

module Render.IncludedPackages where

import Control.Monad
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html4.Strict.Attributes as A

import Render.Base
import Render.HaskellOrg (branding_style, hl_head, hl_footer, navbar_section, banner_left)

import Data.List

import qualified NewReleaseFiles as RF

included_packages_page :: IO Html
included_packages_page = do
  -- let groups = RF.groupBySameYear releases
  contents <- readFile "/Users/erantapaa/work/fix-hp/hp-site/included.html"
  return $ do
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
        preEscapedString contents
      hl_footer

