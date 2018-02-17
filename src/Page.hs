{-# LANGUAGE OverloadedStrings #-}

module Page 
    ( PageType (..)
    , defContext
    , pageCompiler
    , useAbsoluteUrls
    ) where

import Hakyll
import Config

import Data.Time
import Data.List (isPrefixOf)
import Data.Monoid
import System.FilePath
import Control.Monad.Trans.Class (lift)

import Attached
import Compilers

--------------------------------------------------------------------------------

generationDateCtx :: Context String
generationDateCtx =
  field "gendate" (const genDate)
  where
    genDate = do
      time <- unsafeCompiler $ getCurrentTime >>= utcToLocalZonedTime
      return (formatTime defaultTimeLocale "%B %e, %Y at %H:%M" time)

_fragmentsCtx :: Context String
_fragmentsCtx = functionField "markdown" markdown
  where
    markdown args _item =
      case args of
        [] -> error "Missing argument for $markdown"
        arg:_ ->
          let path = "fragments" </> arg in
          itemBody <$> load (fromFilePath path)

defContext :: Context String
defContext = generationDateCtx <> defaultContext

--------------------------------------------------------------------------------

data PageType = Raw | Simple

applyPageTemplate :: PageType -> Item String -> Compiler (Item String)
applyPageTemplate Raw content = return content
applyPageTemplate Simple content =
  loadAndApplyTemplate "templates/page-simple.html" defContext content

pageCompiler :: 
  PageType -> SiteOptions -> Compiler (Item (Attached String))
pageCompiler pageType opts =
  withAttachedFiles $ do
    pandoc <- markdownCompiler opts
    lift ( applyPageTemplate pageType pandoc
      >>= loadAndApplyTemplate "templates/default.html" defContext
      >>= relativizeUrls )

--------------------------------------------------------------------------------

useAbsoluteUrls :: Item String -> Compiler (Item String)
useAbsoluteUrls = withItemBody $ \page -> do
  root <- loadBody "siteroot"
  return (withUrls (absolute root) page)
  where absolute root url =
          if "/" `isPrefixOf` url then root ++ url else url

--------------------------------------------------------------------------------