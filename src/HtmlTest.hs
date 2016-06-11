
-- routines to help test HTML generation

module HtmlTest (
    module TagSoupUtil
  , runDiff
  , readHTML
  , blazeToTrees
  , blazeToString
  , blazeToPrettyString
  , canonicalizeBlaze
) where

import qualified Text.HTML.TagSoup.Tree as TS
import TagSoupUtil
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Temp
import System.Process
import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Blaze.Html.Renderer.Pretty as BlazePretty

-- use diff to report differences between two Strings
runDiff :: String -> String -> IO Bool
runDiff text1 text2 = do
  withSystemTempFile "expected" $ \path1 h1 -> do
    withSystemTempFile "got" $ \path2 h2 -> do
      hClose h1
      hClose h2
      writeFile path1 text1
      writeFile path2 text2
      let cmd = "colordiff -b -u " ++ path1 ++ " " ++ path2
      p <- spawnCommand cmd
      st <- waitForProcess p
      case st of
        ExitSuccess -> return True
        ExitFailure _ -> return False

-- load an HTML file
readHTML :: FilePath -> IO [HtmlTree]
readHTML path = do
  content <- readFile path
  return $ TS.parseTree content

blazeToPrettyString :: Blaze.Html -> String
blazeToPrettyString = BlazePretty.renderHtml

blazeToString :: Blaze.Html -> String
blazeToString html = Blaze.renderHtml html

-- convert a Blaze HTML document to a TagSoup Forest
blazeToTrees :: Blaze.Html -> [HtmlTree]
blazeToTrees html = TS.parseTree (Blaze.renderHtml html)

-- convert Blaze markup to a canonical HTML string
canonicalizeBlaze html = canonicalizeHTML $ Blaze.renderHtml html

