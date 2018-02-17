{-# LANGUAGE OverloadedStrings #-}

module Blog
    ( BlogOptions (..)
    , generateBlog
    ) where

import Hakyll
import Config

import Data.Time
import Control.Monad
import Data.Monoid ((<>))
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad.Trans.Class (lift)

import Attached
import Compilers

--------------------------------------------------------------------------------

data BlogOptions = BlogOptions
    { blogTitle :: String
    , posts :: Pattern
    , indexIdentifier :: Identifier
    , tagsPages :: String -> Identifier
    , indexTemplate :: Identifier
    , tagsPageTemplate :: Identifier
    , nRecentDisplayed :: Int 
    , blogDefContext :: Context String }


postCtx :: BlogOptions -> Tags -> Context String
postCtx opts tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , blogDefContext opts ]


postCompiler :: 
    SiteOptions -> BlogOptions -> Tags -> Compiler (Item (Attached String))
postCompiler site blog tags = 
    withAttachedFiles $ do
    pandoc <- markdownCompiler site
    lift (
        loadAndApplyTemplate "templates/post.html" (postCtx blog tags) pandoc
        >>= loadAndApplyTemplate "templates/default.html" (blogDefContext blog)
        >>= relativizeUrls )


createdFirst :: [Item String] -> Compiler [Item String]
createdFirst items = do
  itemsWithTime <- forM items $ \item -> do
    -- getItemUTC looks for the metadata "published" or "date"
    utcTime <- getItemUTC defaultTimeLocale $ itemIdentifier item
    return (utcTime ,item)
  return $ map snd $ sortBy (flip (comparing fst)) itemsWithTime


renderTagged :: BlogOptions -> String -> Compiler String
renderTagged blog tag = do
    ids <-
        getMatches (posts blog)
        >>= filterM (getTags >=> return . (tag `elem`))
    let its = mapM load' ids >>= createdFirst
    let ctx = listField "posts" (blogDefContext blog) its
    fmap itemBody (
        makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/post-list-simple.html" ctx)


taggedPostsCtx :: BlogOptions -> Context String
taggedPostsCtx blog =
    functionField "tagged" tagged
    where
        tagged args _ =
            case args of
                [tag] -> renderTagged blog tag
                _ -> error "Field $tagged$ must take exactly one argument."


indexPage :: BlogOptions -> Tags -> Compiler (Item String)
indexPage blog tags = do
    recent <- take (nRecentDisplayed blog) <$> 
              (createdFirst =<< loadAll' (posts blog))
    let ctx = constField "title" (blogTitle blog)
           <> listField "posts" (postCtx blog tags) (return recent)
           <> taggedPostsCtx blog
    makeItem ""
        >>= loadAndApplyTemplate (indexTemplate blog) ctx
        >>= loadAndApplyTemplate "templates/default.html" (blogDefContext blog)
        >>= relativizeUrls


generateBlog :: SiteOptions -> BlogOptions -> Rules ()
generateBlog site blog = do
    tags <- buildTags (posts blog) (tagsPages blog)
    match (posts blog) $ do
        route $ setExtension ".html"
        compile $ postCompiler site blog tags
    create [indexIdentifier blog] $ do
        route idRoute
        compile (indexPage blog tags)
    tagsRules tags $ \tag taggedPosts -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            ps <- createdFirst =<< loadAll' taggedPosts
            let ctx = constField "title" title
                   <> constField "tag" tag
                   <> listField "posts" (postCtx blog tags) (return ps)
                   <> blogDefContext blog
            makeItem ""
                >>= loadAndApplyTemplate (tagsPageTemplate blog) ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

--------------------------------------------------------------------------------