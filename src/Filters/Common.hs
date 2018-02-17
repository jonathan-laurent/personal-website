{-# LANGUAGE KindSignatures #-}

module Filters.Common
  ( module X
  , module Hakyll
  , contextFromAttributes
  , contextFromAttributesIn
  , applyExtTemplate
  , Filter
  , makePureFilter
  , makeFilter
  , htmlMarkup
  , escapeHtmlPre
  , readProcess
  , unsafePerformIO
  , catch
  ) where

--------------------------------------------------------------------------------

-- Exported modules

import Data.Monoid                 as X
import Control.Monad.Trans.Class   as X
import Text.Pandoc.Walk            as X
import Text.Pandoc                 as X hiding (Template, applyTemplate)
import Attached                    as X

-- Used internally

import Hakyll

import System.Process (readProcess)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (catch)

import Text.Blaze.Html5 (preEscapedToHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

--------------------------------------------------------------------------------

type Filter (m :: * -> *) = Item Pandoc -> m (Item Pandoc)

makeFilter :: (Monad m) => (Pandoc -> m Pandoc) -> Filter m
makeFilter = traverse

makePureFilter :: (Monad m) => (Pandoc -> Pandoc) -> Filter m
makePureFilter f = makeFilter (return . f)

--------------------------------------------------------------------------------

htmlMarkup
  :: String -> (String, [String], [(String, String)]) -> String -> String
htmlMarkup name (id, classes, attributes) body =
  if body /= "" then
    "<" ++ name ++ " " ++ attStr ++ " >\n" ++ body ++ "\n</" ++ name ++ ">\n"
  else
    "<" ++ name ++ " " ++ attStr ++ " />\n"
  where
    classesStr = unwords classes
    idAttr = [("id", id) | id /= ""]
    attrs = idAttr ++ [("class", classesStr) | classesStr /= ""] ++ attributes
    renderPair (key, val) = key ++ "=\"" ++ val ++ "\""
    attStr = unwords (map renderPair attrs)

--------------------------------------------------------------------------------

escapeHtmlPre :: String -> String
escapeHtmlPre s = renderHtml (preEscapedToHtml s)

contextFromAttributes :: [(String, String)] -> Context String
contextFromAttributes attrs = mconcat [constField k v | (k, v) <- attrs]

contextFromAttributesIn :: [String] -> [(String, String)] -> Context String
contextFromAttributesIn attrs pairs =
  contextFromAttributes (filter ((`elem` attrs) . fst) pairs)

applyExtTemplate :: String -> Context String -> String -> Compiler String
applyExtTemplate templateFile context code = do
  let tmpl = (readTemplate . unsafePerformIO . readFile) templateFile
  item <- makeItem code
  itemBody <$> applyTemplate tmpl (context <> defaultContext) item

--------------------------------------------------------------------------------
