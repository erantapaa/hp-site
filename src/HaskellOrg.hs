{-# LANGUAGE OverloadedStrings #-}

module HaskellOrg where

import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A

{-
    <head>
       hl_head
    <body>
      <div class="wrap">
        ... navbar_section ...

        <div class="header"> ...  </div>
        <br>
        <br class="hidden-xs hidden-sm">
        <br class="hidden-xs hidden-sm">

        <div class="pattern-bg"> ... </div>
        <br>
        ...
      </div>                 <!-- ends class="wrap" -->
      <div class="footer"> ... </div>
      ... scripts ...
    </body>

-}

hl_head = do
    base ! href "http://haskell.org"
    H.title "Haskell Language"
    meta ! charset "utf-8"
    meta ! content "IE edge" ! httpEquiv "X-UA-Compatible"
    meta ! content "width=device-width, initial-scale=1" ! name "viewport"
    meta ! content "haskell,functional,pure,programming,lazy" ! name "keywords"
    meta ! content "The Haskell purely functional programming language home page." ! name "description"
    link ! href "/static/img/favicon.ico?etag=-4XQV6tt" ! rel "shortcut icon"
    link ! href "https://fonts.googleapis.com/css?family=Source+Sans+Pro|Raleway:700,900|Ubuntu+Mono:400" ! type_ "text/css" ! rel "stylesheet"
    link ! href "/static/css/hl.min.css?etag=2LtRot0K" ! type_ "text/css" ! rel "stylesheet"

navbar_section = do
    nav ! class_ "navbar navbar-default" $ H.div ! class_ "container" $ do
        H.div ! class_ "navbar-header" $ button ! dataAttribute "toggle" "collapse" ! dataAttribute "target" "#haskell-menu" ! class_ "navbar-toggle collapsed" $ do
            H.span ! class_ "sr-only" $ mempty
            H.span ! class_ "icon-bar" $ mempty
            H.span ! class_ "icon-bar" $ mempty
            H.span ! class_ "icon-bar" $ mempty
        H.div ! A.id "haskell-menu" ! class_ "collapse navbar-collapse" $ ul ! class_ "nav navbar-nav navbar-right" $ do
            li $ a ! href "/downloads" $ "Downloads"
            li $ a ! href "/community" $ "Community"
            li $ a ! href "/documentation" $ "Documentation"
            li $ a ! href "/news" $ "News"


section_wrapper x = do
  H.div ! class_ "container" $ do
    H.div ! class_ "row" $ do
      H.div ! class_ "span12 col-sm-12" $ do
        H.div ! class_ "container" $ do
          H.div ! class_ "row" $ x

