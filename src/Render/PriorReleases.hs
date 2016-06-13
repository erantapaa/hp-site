{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Prior Releases page

module Render.PriorReleases where

import Control.Monad
import qualified Render.Base as RB
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html4.Strict.Attributes as A

import Render.Base
import Render.HaskellOrg (branding_style, hl_head, hl_footer, navbar_section, banner_left)

import Data.List
import Data.Ord (comparing)
import Data.Function (on)

import ReleaseFiles (DistType, distName)

-- ReleaseFiles = (Version, (month, year), files)

months :: [String]
months = words "January February March April May June July August September October November December"

releasesByYear :: [ReleaseFiles] -> [(Int,[ReleaseFiles])]
releasesByYear releases = 
    map (\xs -> ( fst . Prelude.head $ xs, map snd xs ))
    $ groupBy (on (==) fst)
    $ map dropMonth
    $ sortBy (flip (comparing fst))
    $ [ ((y,m), r) | r@(_,(m,y),_) <- releases ]
  where dropMonth ((y,m),r) = (y,r)

--  map snd $ groupBy fst $ map dropMonth $ sort [ ((y,m), r) | r@(_,(m,y),_) <- releases ]

getDist :: FileInfo -> DistType
getDist (a,_,_,_) = a

-- getUrl :: FileInfo -> Url
getUrl (_,u,_,_) = u

renderFileInfo :: Int -> FileInfo -> String
renderFileInfo year fileinfo
  | year >= 2016 = "???"
  | otherwise    = distName (getDist fileinfo)

renderRelease (version, (month, year), files) = do
  let monthName = months !! (month-1)

  p $ do strong (toMarkup version)
         ", " >> toMarkup monthName >> " " >> toMarkup year
         sequence_
           $ intersperse " - "
           $ [ a ! href (stringValue (getUrl file)) $ toMarkup (renderFileInfo year file) | file <- files ]

prior_releases_page :: [ReleaseFiles] -> Html
prior_releases_page releases = do
  let groups = releasesByYear releases
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
        forM_ groups $ \(year, rels) -> do
          h3 ! A.id "section" $ toMarkup year
          forM_ rels $ \rel -> do renderRelease rel
    hl_footer

