module ParseFAQ
where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Show.Pretty

import Control.Monad
import qualified Text.Blaze.Html5 as H hiding (map)
import qualified Text.Blaze.Html4.Strict.Attributes as A

{-

The point of the module is to allow one to put the FAQ questions
and answers in an HTML file which looks like:

    <question>How do I uninstall the Haskell Platform?</question>
    <answer>...</answer>
    <question>Where are components located?</question>
    <answer>...</answer>

and produce both a contents list and a body section.

-}

-- parse <question>...</question><answer></answer>
--
-- keep only TagBranch elements
-- expect: name="question", name="answer", ...

doit = do
  contents <- readFile "input.html"
  putStrLn $ ppShow $ parseTree contents

doit2 = do
  contents <- readFile "input.html"
  let qapairs = parseQApairs $ onlyQABranches $ parseTree contents
  putStrLn $ concatMap renderQApair qapairs

doit3 = renderFaqContentsBody "input.html"

-- read QAPairs from a file and return the rendered contents list
-- and FAQ body
renderFaqContentsBody :: FilePath -> IO (H.Html, String)
renderFaqContentsBody path = do
  contents <- readFile path
  let qapairs = assignNames $ parseQApairs $ onlyQABranches $ parseTree contents
      index = H.ul $ do
                forM_ qapairs $ \qa@(QAPair q a) -> do
                  let name = H.toValue ("#" ++ qaName qa)
                      body = H.preEscapedString (qaTitle qa)
                  H.li $ H.a H.! A.href name $ body
      body = concatMap renderQApair qapairs
  return (index, body)

type Element = TagTree String

emptyAnswer = TagBranch "answer" [] []

data QABranch = QBranch Element | ABranch Element

data QAPair = QAPair Element Element

isBranch (TagBranch _ _ _) = True
isBranch _                 = False

-- is a TagTree a branch with a specific name
isBranchNamed str (TagBranch str' _ _) = str == str'
isBranchNamed _   _                    = False

-- reduce a list of Elements to a list of QABranches
onlyQABranches :: [Element] -> [QABranch]
onlyQABranches [] = []
onlyQABranches (t:ts)
  | isBranchNamed "question" t = QBranch t : onlyQABranches ts
  | isBranchNamed "answer" t   = ABranch t : onlyQABranches ts
  | otherwise                  = onlyQABranches ts

parseQApairs :: [QABranch] -> [QAPair]
parseQApairs [] = []
parseQApairs (ABranch _ : ts) = parseQApairs ts
parseQApairs [QBranch e] = [QAPair e emptyAnswer ]
parseQApairs ts@(QBranch e : QBranch _ : _) = (QAPair e emptyAnswer) : parseQApairs (tail ts)
parseQApairs (QBranch e : ABranch f : ts) = (QAPair e f) : parseQApairs ts

-- render just the question body as HTML
qaTitle :: QAPair -> String
qaTitle (QAPair q a) = renderTree qbody
  where (TagBranch _ _ qbody) = q

qaName :: QAPair -> String
qaName (QAPair q a) = lookupAttr "" "name" qattrs
  where (TagBranch _ qattrs _) = q

-- assign sequential names to a list of QAPairs
assignNames :: [QAPair] -> [QAPair]
assignNames qapairs = zipWith go [(1::Int)..] qapairs
  where go i qa = assignName ("faq-"++show i) qa

-- set the name attribute
assignName :: String ->  QAPair -> QAPair
assignName name (QAPair q a) = QAPair q' a
  where TagBranch qtag qattrs qbody = q
        q' = TagBranch qtag qattrs' qbody
        qattrs' = [("name", name)] ++ qattrs

-- lookup an attribute
lookupAttr :: String -> String -> [Attribute String] -> String 
lookupAttr def key attrs = maybe def id (lookup key attrs)

-- render a QAPair to HTML
renderQApair :: QAPair -> String
renderQApair (QAPair q a) = 
  let TagBranch _ qattrs qbody = q
      TagBranch _ aattrs abody = a
      qattrs' = [("class", "faq-question")] ++ qattrs

      name = lookupAttr "" "name" qattrs
      addAnchor = if null name then id else (\x -> (TagBranch "a" [ ("name", name) ] []):x)
      q' = TagBranch "div" qattrs' (addAnchor qbody)

      aattrs' = [("class", "faq-answer")] ++ aattrs
      a' = TagBranch "div" aattrs' abody
      newline = TagLeaf (TagText "\n")
  in renderTree [q', newline, a', newline]

