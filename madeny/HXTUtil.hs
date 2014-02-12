module HXTUtil where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List
import Data.Maybe
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Control.Monad.Trans.Maybe
import Text.XML.HXT.Core		-- basic HXT stuff
import Text.XML.HXT.XPath               -- additional XPath functions
import Text.XML.HXT.Curl                -- Curl HTTP handler

toAbsHRefs	:: IOStateArrow s XmlTree XmlTree
toAbsHRefs
    = ( mkAbsHRefs $< computeBaseRef )
      >>>
      removeBaseElement

removeBaseElement	:: ArrowXml a => a XmlTree XmlTree
removeBaseElement
    = processChildren
      ( processChildren ( none
			  `when`
			  ( isElem >>> hasName "base" )
			)
       `when`
       ( isElem >>> hasName "head" )
      )

mkAbsHRefs	:: ArrowXml a => String -> a XmlTree XmlTree
mkAbsHRefs base
    = processTopDown editHRef
    where
    editHRef
	= processAttrl ( changeAttrValue (absHRef base)
			 `when`
			 hasName "href"
		       )
	  `when`
	  ( isElem >>> hasName "a" )
	where

	absHRef	:: String -> String -> String
	absHRef base url
	    = fromMaybe url . expandURIString url $ base

toAbsRefs	:: IOStateArrow s XmlTree XmlTree
toAbsRefs
    = ( mkAbsRefs $< computeBaseRef )
      >>>
      removeBaseElement

mkAbsRefs0	:: ArrowXml a => String -> a XmlTree XmlTree
mkAbsRefs0 base
    = processTopDown ( editRef "a" "href"
		       >>>
		       editRef "img" "src"
		       >>>
		       editRef "link" "href"
		       >>>
		       editRef "script" "src"
		     )
    where
    editRef en an
	= processAttrl ( changeAttrValue (absHRef base)
			 `when`
			 hasName an
		       )
	  `when`
	  ( isElem >>> hasName en )
	where

	absHRef	:: String -> String -> String
	absHRef base url
	    = fromMaybe url . expandURIString url $ base

mkAbsRefs	:: ArrowXml a => String -> a XmlTree XmlTree
mkAbsRefs base
    = processTopDown editRefs
    where
    editRefs
	= seqA . map (uncurry editRef)
	  $
	  [ ("a", "href")
	  , ("img", "src")
	  , ("link", "href")
	  , ("script", "src")	    -- and more
	  ]

    editRef en an
	= processAttrl ( changeAttrValue (absHRef base)
			 `when`
			 hasName an
		       )
	  `when`
	  ( isElem >>> hasName en )
	where

	absHRef	:: String -> String -> String
	absHRef base url
	    = fromMaybe url . expandURIString url $ base

computeBaseRef	:: IOStateArrow s XmlTree String
computeBaseRef
    = ( ( ( isElem >>> hasName "html"
	    >>>
	    getChildren
	    >>>
	    isElem >>> hasName "head"
	    >>>
	    getChildren
	    >>>
	    isElem >>> hasName "base"
	    >>>
	    getAttrValue "href"
	  )
	  &&&
	  getBaseURI
	)
	>>> expandURI
      )
      `orElse` getBaseURI


getDescendends :: ArrowXml a => [String] -> a XmlTree XmlTree
getDescendends
    = foldl1 (\ x y -> x >>> getChildren >>> y)
      .
      map (\ n -> isElem >>> hasName n)

computeBaseRef1	:: IOStateArrow s XmlTree String
computeBaseRef1
    = ( ( ( getDescendends ["html","head","base"]
	    >>>
	    getAttrValue "href"
	  )
	  &&&
	  getBaseURI
	)
	>>> expandURI
      )
      `orElse` getBaseURI

computeBaseRef2	:: IOStateArrow s XmlTree String
computeBaseRef2
    = ( ( xshow (getXPathTrees "/html/head/base@href")
	  &&&
	  getBaseURI
	)
	>>> expandURI
      )
      `orElse` getBaseURI

toAbsRefs1	:: IOStateArrow s XmlTree XmlTree
toAbsRefs1
    = ( mkAbsRefs $< computeBaseRef1 )
      >>>
      removeBaseElement
