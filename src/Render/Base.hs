{-# LANGUAGE OverloadedStrings #-}

module Render.Base (
  FileInfo
  , RF.OS(..)
  , RF.Release
  , binsFor, srcDists
  , downloadButton, hashRow, downloadButtonsAndHashes
  , hl_src, hl_href, expander
  , distro_svg, distro_png, DistroIcon(..), distro_button_list, distro_button
  , section_hrule
)
where

import Control.Monad
import Data.Monoid
import qualified NewReleaseFiles as RF
import NewReleaseFiles (OS(..), FileInfo(..))
import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

binsFor :: OS -> [FileInfo] -> [FileInfo]
binsFor = filter . RF.isBinaryFor 

srcDists :: [FileInfo] -> [FileInfo]
srcDists = filter RF.isSource

minfull :: FileInfo -> String
minfull = RF.varpart . RF._variant

-- label to use for the download button
buttonLabel :: FileInfo -> String
buttonLabel file = "Download " ++ RF.buttonLabel file

-- label to use for SHA256 hashes
hashLabel :: FileInfo -> String
hashLabel file = RF.hashLabel file

-- HTML rendering functions

hl_href url = href ("http://haskell.org/" <> url)
hl_src url = src ("http://haskell.org/" <> url)

expander anchor ident = do
      let downArrow = hl_src "platform/img/expand-piece.svg"
      a ! A.id ident ! class_ "expander" ! href (stringValue anchor) $ H.div $ do
          img ! downArrow ! class_ "expand-1"
          img ! downArrow ! class_ "expand-2"
          img ! downArrow ! class_ "expand-3"

downloadButton :: FileInfo -> Html
downloadButton rfile = do
    let url = RF._url rfile
    H.div ! class_ "download-btn" $ do
        a ! href (stringValue url)
          ! onclick "return dl(this)"
          {- role = "button" -}
          ! class_ "btn btn-haskell" $ do
            i ! class_ "fa fa-download" $ mempty
            toMarkup (buttonLabel rfile)

hashRow :: FileInfo -> Html
hashRow rfile = do
    let hash = maybe "---" Prelude.id (RF._hash rfile)
    li $ do
        H.span $ toMarkup (hashLabel rfile)
        input ! readonly "" ! class_ "file-hash" ! type_ "text" ! value (stringValue hash)

downloadButtonsAndHashes :: [FileInfo] -> Html
downloadButtonsAndHashes files = do
  forM_ files downloadButton
  H.div $ do
      strong "SHA-256"
      " hashes:"
      ul ! class_ "hashes" $ do
          forM_ files hashRow

data DistroIcon = Image { _img_alt :: String, _img_url :: String }
                    | FontAwesome { _fa_class :: String }

distro_svg :: String -> String
distro_svg dist = "http://haskell.org/platform/img/distro-" ++ dist ++ ".svg"

distro_png :: String -> String
distro_png dist = "http://haskell.org/platform/img/distro-" ++ dist ++ ".png"

distro_button :: (String, String, DistroIcon) -> Html
distro_button (flavor, name, icon) = do
    let anchor = "#" ++ flavor
    li ! class_ "flavor-li" ! dataAttribute "flavor" (stringValue flavor) $ do
        a ! href (stringValue anchor) ! dataAttribute "flavor" (stringValue flavor) $ do
            case icon of
              FontAwesome cls -> do
                  H.span ! class_ "logo" $ i ! class_ (stringValue cls) $ mempty
                  toMarkup name
              Image altlabel url -> do
                  img ! alt (stringValue altlabel) ! class_ "logo" ! src (stringValue url)
                  toMarkup name

distro_button_list :: [(String,String,DistroIcon)] -> Html
distro_button_list icons = do
    ul ! class_ "choose-distrbution" $ do
        forM_ icons distro_button

step stepno body = do
    li $ do
        H.div ! class_ "step-number" $ (toMarkup stepno)
        H.div ! class_ "step-body" $ (toMarkup body)

section_hrule = do
    H.div ! class_ "bottom-rule" $ mempty

-- --

-- XXX currently not exported and duplicated in Render.DownloadPage
hp_head = do
    link ! href  "801/download.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css" ! rel "stylesheet" ! type_ "text/css"
    H.style $ do
      ".hp-branding { font-family: sans-serif; line-height: 50px; font-weight: bold; font-size: 50px; background-repeat: no-repeat; background-size: 70px; display: block; padding-left: 80px; background-position: left; } "
      ".hp-summary { margin-top: 20px; display: block; font-size: 20px; }"
    script ! hl_src "/platform/js/jquery-1.11.1.min.js" $ mempty
    script ! src "801/download.js" $ mempty

-- ---
