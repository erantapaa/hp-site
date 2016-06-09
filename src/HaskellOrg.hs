{-# LANGUAGE OverloadedStrings #-}

module HaskellOrg where

import Text.Blaze.Html5 as H
import Text.Blaze.Html4.Strict.Attributes as A
import Data.Monoid

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

hl_href url = href ("http://haskell.org/" <> url)
hl_src url = src ("http://haskell.org/" <> url)

hl_head = do
    meta ! charset "utf-8"
    meta ! content "IE edge" ! httpEquiv "X-UA-Compatible"
    meta ! content "width=device-width, initial-scale=1" ! name "viewport"
    meta ! content "haskell,functional,pure,programming,lazy" ! name "keywords"
    meta ! content "The Haskell purely functional programming language home page." ! name "description"
    link ! hl_href "/static/img/favicon.ico?etag=-4XQV6tt" ! rel "shortcut icon"
    link ! href "https://fonts.googleapis.com/css?family=Source+Sans+Pro|Raleway:700,900|Ubuntu+Mono:400" ! type_ "text/css" ! rel "stylesheet"
    link ! hl_href "/static/css/hl.min.css?etag=2LtRot0K" ! type_ "text/css" ! rel "stylesheet"

navbar_section = do
    nav ! class_ "navbar navbar-default" $ H.div ! class_ "container" $ do
        H.div ! class_ "navbar-header" $ button ! dataAttribute "toggle" "collapse" ! dataAttribute "target" "#haskell-menu" ! class_ "navbar-toggle collapsed" $ do
            H.span ! class_ "sr-only" $ mempty
            H.span ! class_ "icon-bar" $ mempty
            H.span ! class_ "icon-bar" $ mempty
            H.span ! class_ "icon-bar" $ mempty
        H.div ! A.id "haskell-menu" ! class_ "collapse navbar-collapse" $ ul ! class_ "nav navbar-nav navbar-right" $ do
            li $ a ! hl_href "/downloads" $ "Downloads"
            li $ a ! hl_href "/community" $ "Community"
            li $ a ! hl_href "/documentation" $ "Documentation"
            li $ a ! hl_href "/news" $ "News"


hl_footer = do
    H.div ! class_ "footer" $ H.div ! class_ "container" $ do
        p mempty
        H.div ! class_ " container " $ H.div ! class_ " row " $ do
            H.div ! class_ " span3 col-sm-4 col-md-3" $ H.span ! class_ "item" $ "© 2014–2016 haskell.org"
            H.div ! class_ " span12 col-xs-12 visible-xs" $ br
            H.div ! class_ " span8 col-sm-4 col-md-6 text-center" $ do
                br ! class_ "visible-xs"
                H.span ! class_ "item" $ "Got changes to contribute?"
                br ! class_ "visible-xs"
                a ! href "https://github.com/haskell-infra/hl" $ "Fork or comment on Github"
                br ! class_ "visible-xs"
            H.div ! class_ " span12 col-xs-12 visible-xs" $ br
            H.div ! class_ " span3 col-sm-4 col-md-3 text-right" $ do
                H.span "Proudly hosted by"
                a ! href "https://www.rackspace.com/" $ img ! height "20" ! width "20" ! hl_src "/static/img/rackspace.svg?etag=J84VdDuP" ! alt "rackspace"
            H.div ! class_ " span12 col-sm-12" $ br
        p mempty

