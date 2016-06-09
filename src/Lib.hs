{-# LANGUAGE OverloadedStrings #-}

module Lib where

import HaskellOrg
import HaskellPlatform

import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

import qualified Text.Blaze.Html.Renderer.Pretty as RP
import qualified Text.Blaze.Html.Renderer.String as RS

page1 = do
  H.head $ do
    hl_head
  body $ do
    mempty

page2 = do
  H.head $ do
    hl_head
    hp_head
  body $ mempty

page3 = do
  H.head $ do
    hl_head
    H.title "Download the Haskell Platform"
    hp_head
  body ! class_ "page-home" $ do
    H.div ! class_ "wrap" $ do
      navbar_section
      -- the banner area
      H.div ! class_ "pattern-bg" $ do
        H.div ! class_ "container" $ do
          H.div ! class_ "row" $ do
            H.div ! class_ "span6 col-sm-6" $ do
              banner_left
            H.div ! class_ "span6 col-sm-6" $ do
              banner_right
      -- getting started
      H.div ! class_ "container" $ do
        H.div ! class_ "row" $ do
          H.div ! class_ "span12 col-sm-12" $ do
            getting_started
            found_user_platform
            unknown_user_platform
            platform_toc
            H.div ! class_ "container" $ do
              linux_download
              osx_download
              windows_download
    hl_footer  -- same level as the "wrap" class div

pretty page = do
  writeFile "pretty-out.html" $ RP.renderHtml page
  putStrLn "output written to pretty-out.html"

render page = do
  writeFile "out.html" $ RS.renderHtml page
  putStrLn "output written to out.html"

