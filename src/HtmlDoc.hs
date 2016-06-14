
module HtmlDoc
  (HtmlDoc, Html, module D, renderText, renderLazyText, assembleHtml)
where

import Data.Monoid
import Doc as D

import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html.Renderer.Text as BlazeT

type HtmlDoc = Doc Html

assembleHtml :: HtmlDoc -> Html
assembleHtml doc = do
  H.head $ _header doc
  H.body $ _body doc >> _end doc

-- render as strict Text
renderText :: HtmlDoc -> T.Text
renderText doc  =  TL.toStrict $ BlazeT.renderHtml (assembleHtml doc)

-- render as lazy Text
renderLazyText :: HtmlDoc -> TL.Text
renderLazyText doc  =  BlazeT.renderHtml (assembleHtml doc)

