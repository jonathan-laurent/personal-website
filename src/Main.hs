{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default
import Control.Monad
import System.FilePath
import Data.Maybe (isJust)
import System.Process (callCommand)
import System.Environment (getArgs, lookupEnv)

import Hakyll

import Attached
import Filters
import Compilers
import Page
import Blog
import Config

--------------------------------------------------------------------------------

runHakyll :: SiteOptions -> IO ()
runHakyll opts = hakyllWith hakyllConfig $ do

  let imagesToCopy = "img/**" .&&. complement "img/bw/**"
  let toCopy = "katex/**" .||. "fonts/**" .||. "pdf/**" .||. imagesToCopy
  match toCopy $ do
    route idRoute
    compile copyFileCompiler

  match "img/bw/**" $ do
    route idRoute
    compile bwImageCompiler

  match ("math/*" .||. "*.txt" .||. "siteroot") $
    compile getResourceString

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/*.scss" $ do
    route (setExtension "css")
    compile compressScssCompiler

  createPygmentStyle "css/pygment.css" (pygmentStyle opts)

  match "templates/*" $ compile templateCompiler

  match "fragments/*" $
    compile $ dropAttachedFiles (markdownCompiler opts)

  match "software/*.md" $ do
    route $ setExtension ".html"
    compile $ pageCompiler Simple opts

  mapM_ (generateBlog opts) [blogOptions, notesOptions, draftsOptions]

  let errorPages = "404.md" .||. "forbidden.md"

  match ("*.md" .&&. complement errorPages) $ do
    route $ setExtension ".html"
    compile $ pageCompiler Raw opts

  -- Render the error pages: we don't relativize URLs here
  match errorPages $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defContext
        >>= useAbsoluteUrls

--------------------------------------------------------------------------------

blogOptions :: BlogOptions
blogOptions = BlogOptions
  { blogTitle = "Blog"
  , posts = "blog/**.md"
  , tagsPages = fromCapture "tags/blog/*.html"
  , blogDefContext = defContext
  , indexIdentifier = "blog.html"
  , indexTemplate = "templates/blog-index.html"
  , tagsPageTemplate = "templates/blog-tag.html" 
  , nRecentDisplayed = 5 }

notesOptions :: BlogOptions
notesOptions = BlogOptions
  { blogTitle = "Notes"
  , posts = "notes/**.md"
  , tagsPages = fromCapture "tags/notes/*.html"
  , blogDefContext = defContext
  , indexIdentifier = "notes.html"
  , indexTemplate = "templates/notes-index.html"
  , tagsPageTemplate = "templates/notes-tag.html"
  , nRecentDisplayed = 5 }

draftsOptions :: BlogOptions
draftsOptions = BlogOptions
  { blogTitle = "Drafts"
  , posts = "drafts/**.md"
  , tagsPages = fromCapture "tags/drafts/*.html"
  , blogDefContext = defContext
  , indexIdentifier = "drafts.html"
  , indexTemplate = "templates/drafts-index.html"
  , tagsPageTemplate = "templates/drafts-tag.html"
  , nRecentDisplayed = 30 }

--------------------------------------------------------------------------------

copyHtAccess :: IO ()
copyHtAccess = do
  args <- getArgs
  case args of
    mode : _ ->
      when (mode `elem` ["watch", "build", "rebuild", "deploy"]) $ do
        putStrLn "Copying .htaccess files"
        mapM_ copy [".htaccess", ".htpasswd", "drafts/.htaccess"]
    _ -> return ()
  where 
    copy f = do
      callCommand ("mkdir -p " ++ takeDirectory dst)
      callCommand ("cp " ++ src ++ " " ++ dst)
      where
        src = providerDirectory hakyllConfig </> f
        dst = destinationDirectory hakyllConfig </> f


main :: IO ()
main = do
  putStrLn "Generating website."
  isQuickPreview <- isJust <$> lookupEnv "PREVIEW"
  let opts = def { quickPreview = isQuickPreview }
  copyHtAccess
  runHakyll opts

--------------------------------------------------------------------------------