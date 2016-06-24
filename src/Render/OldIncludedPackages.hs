
{-# LANGUAGE OverloadedStrings #-}

-- HTML generation routines for the Included Packages page

module Render.IncludedPackages where

import Control.Monad
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html4.Strict.Attributes as A

import Render.Base

import Data.List
import qualified Data.List.Ordered as Ordered
import Data.Char

import qualified NewReleaseFiles as RF

import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Function

data Package = Package { _pkg_name :: String, _pkg_version :: String, _pkg_url :: String }
  deriving (Show, Read)

data Cell =  Missing | Available String Package
  deriving (Show,Read)

sortOnLower = sortOn (map toLower)

data ParsedVersion = Unknown | Ok Version
  deriving (Show,Read,Eq,Ord)

readVersion :: String -> ParsedVersion
readVersion str =
  let parses =  (readP_to_S parseVersion) str
  in
  case parses of
    []        -> Unknown
    ((v,_):_) -> Ok v

-- return the latest version of a package
latestVersion :: [Package] -> Package
latestVersion packages = maximumBy (on compare (readVersion . _pkg_version)) packages

organizePackages :: [[Package]] -> [[Cell]]
organizePackages pkgss =
  let -- the names ordered by package 
      ordered = map (sort . map _pkg_name) pkgss
      -- all package names in alphabetical order
      allnames = foldr1 Ordered.union ordered
  in []

tableHeader releases = do
  thead $ tr $ do
    th mempty
    forM_ releases $ \rel -> do
      th ! class_ "version" $ toMarkup (RF._rls_name rel)

