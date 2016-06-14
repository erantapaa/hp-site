
module Doc
  (Doc(..), emptyDoc, prependHead, appendHead, prependBody, appendBody, prependEnd, appendEnd)
where

import Data.Monoid

data Doc a = Doc { _header :: a , _body   :: a , _end    :: a }

emptyDoc :: Monoid a => Doc a
emptyDoc = Doc mempty mempty mempty

prependHead :: Monoid a => Doc a -> a -> Doc a
prependHead doc a = doc { _header = a <> _header doc }

appendHead :: Monoid a => Doc a -> a -> Doc a
appendHead doc a = doc { _header = _header doc <> a }

prependBody :: Monoid a =>  Doc a -> a -> Doc a
prependBody doc a = doc { _body = a <> _body doc }

appendBody :: Monoid a => Doc a -> a -> Doc a
appendBody doc a = doc { _body = _body doc <> a }

prependEnd :: Monoid a => Doc a -> a -> Doc a
prependEnd doc a = doc { _end = a <> _end doc }

appendEnd :: Monoid a => Doc a -> a -> Doc a
appendEnd doc a = doc { _end = _end doc <> a }

