
-- routines to help test HTML generation

module HtmlTest (
    module TagSoupUtil
  , runDiff
  , readHTML
  , blazeToTrees
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

-- use diff to report differences between two Strings
runDiff :: String -> String -> IO Bool
runDiff text1 text2 = do
  withSystemTempFile "expected" $ \path1 h1 -> do
    withSystemTempFile "got" $ \path2 h2 -> do
      writeFile path1 text1
      writeFile path2 text2
      let cmd = "diff -b -u " ++ path1 ++ " " ++ path2
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

-- convert a Blaze HTML document to a TagSoup Forest
blazeToTrees :: Blaze.Html -> [HtmlTree]
blazeToTrees html = TS.parseTree (Blaze.renderHtml html)

-- convert Blaze markup to a canonical HTML string
canonicalizeBlaze html = canonicalizeHTML $ Blaze.renderHtml html

