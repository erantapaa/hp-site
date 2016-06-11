module TagSoupUtil (
  HtmlTree,
  canonicalizeTrees,
  canonicalizeHTML,
  prettyTrees,
  markedTrees
) where

import qualified Text.HTML.TagSoup.Tree as TS
import Text.HTML.TagSoup.Tree (TagTree(..))
import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Entity (escapeXML)
import Data.List
import qualified Text.PrettyPrint as PP
import qualified Data.Char as Char

type HtmlTree = TS.TagTree String

-- remove all TagComment tags from an HtmlTree

removeComments :: HtmlTree -> [HtmlTree]
removeComments (TagBranch n a children) = [ TagBranch n a children' ]
  where children' = concatMap removeComments children
removeComments x@(TagLeaf t) =
  case t of
    TagComment _  -> []
    _             -> [x]

-- markedTrees - return a list of marked sub-trees
markedTrees :: HtmlTree -> [(String, HtmlTree)]
markedTrees = filter wanted . markedBranches
  where wanted (c, _) = isPrefixOf "@@" c
        fixup (c, tree) = (drop 2 c, tree)

-- markedBranches - return each instance of a branch following a comment
markedBranches :: HtmlTree -> [ (String, HtmlTree) ]
markedBranches (TagBranch n a children) = concatMap go (tails children)
  where go [] = []
        go ((TagBranch _ _ _):_) = []
        go ((TagLeaf t):rest) =
          case t of
           TagComment c  -> fmap (\x -> (trimSpaces c,x)) $ nextBranch rest
           _             -> []
markedBranches (TagLeaf t) = []

nextBranch :: [HtmlTree] -> [HtmlTree]
nextBranch (x@(TagBranch _ _ _):_) = [x]
nextBranch (TagLeaf (TagOpen _ _):_) = []
nextBranch (TagLeaf (TagComment _):_) = []
nextBranch (x:xs)                   = nextBranch xs

trimSpaces :: String -> String
trimSpaces = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

trim :: String -> String
trim = filter (/= '\n') . trimSpaces

toLower :: String -> String
toLower = map Char.toLower

-- formatting routines

formatAttr :: (String,String) -> String
formatAttr (name, value) = " " ++ toLower name ++ "=\"" ++ escapeXML value ++ "\""

formatAttrs :: [(String,String)] -> String
formatAttrs pairs = concatMap formatAttr . sort . map (\(x,c) -> (toLower x, c)) $ pairs
  -- convert each attr name to lower case, sort and then format

formatTag :: String -> [(String,String)] -> String
formatTag name attrs = "<" ++ toLower name ++ formatAttrs attrs ++ ">"

prettyTree :: HtmlTree -> PP.Doc
prettyTree (TagLeaf t) =
  case t of
    TagText txt       -> let txt' = trim txt in
                         if null txt' then PP.empty
                                      else PP.text (escapeXML txt')
    TagOpen tag attrs -> PP.text $ formatTag tag attrs
    TagComment txt    -> PP.text $ "<!--" ++ txt ++ "-->"
    TagClose tag      -> PP.text $ "</" ++ tag ++ ">"        -- shouldn't happen
    _                 -> PP.empty
prettyTree (TagBranch tag attrs children) =
    PP.text ( formatTag tag attrs )
     PP.$$ PP.nest 4 (PP.vcat (map prettyTree children))
     PP.$$ PP.text ("</" ++ tag ++ ">")

prettyTrees :: [HtmlTree] -> PP.Doc
prettyTrees = PP.vcat . map prettyTree

-- comparison routines

-- convert a forest to a canonical String for diffing
canonicalizeTrees :: [HtmlTree] -> String
canonicalizeTrees = PP.render . prettyTrees . concatMap removeComments 

-- convert HTML text to a canonical version
canonicalizeHTML :: String -> String
canonicalizeHTML = canonicalizeTrees . TS.parseTree

