{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll

import Attached
import Filters
import Compilers
import Page
import Blog
import Config
import Utils

import Data.Default
import Control.Monad
import System.FilePath
import System.Process (callCommand)
import System.Environment (getArgs, lookupEnv)

--------------------------------------------------------------------------------

runHakyll :: SiteOptions -> IO ()
runHakyll opts = hakyllWith hakyllConfig $ do

  let toCopy = "katex/**" .||. "fonts/**" .||. "pdf/**" .||. "downloads/**" .||. "img/**"
  match toCopy $ do
    route idRoute
    compile copyFileCompiler

  match ("math/*" .||. "*.txt" .||. "siteroot") $
    compile getResourceString

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "css/*.scss" $ do
    route (setExtension "css")
    compile compressScssCompiler

  match "css/style/**.scss" $
    -- We do not have to read these files but we track them as dependencies
    compile $ makeItem ()

  createPygmentStyle "css/pygment.css" (pygmentStyle opts)

  match "templates/**" $
    compile templateCompiler

  mapM_ (generateBlog opts) [blogOptions, notesOptions, draftsOptions]

  match "content/**.md" $
    compile $ dropAttachedFiles (markdownCompiler opts)

  match "content/**.yaml" $
    compile getResourceBody


  match "software/*.md" $ do
    route $ setExtension ".html"
    compile $ simpleMarkdownPageCompiler opts

  match "*.html" $ do
    route idRoute
    compile pageCompiler


--------------------------------------------------------------------------------

blogOptions :: BlogOptions
blogOptions = BlogOptions
  { blogTitle = "Blog"
  , posts = "blog/**.md"
  , tagsPages = fromCapture "tags/blog/*.html"
  , blogDefContext = baseContext
  , indexIdentifier = "blog.html"
  , indexTemplate = "templates/blog/index.html"
  , tagsPageTemplate = "templates/blog/tag.html"
  , nRecentDisplayed = 5 }

notesOptions :: BlogOptions
notesOptions = BlogOptions
  { blogTitle = "Notes"
  , posts = "notes/**.md"
  , tagsPages = fromCapture "tags/notes/*.html"
  , blogDefContext = baseContext
  , indexIdentifier = "notes.html"
  , indexTemplate = "templates/notes/index.html"
  , tagsPageTemplate = "templates/notes/tag.html"
  , nRecentDisplayed = 5 }

draftsOptions :: BlogOptions
draftsOptions = BlogOptions
  { blogTitle = "Drafts"
  , posts = "drafts/**.md"
  , tagsPages = fromCapture "tags/drafts/*.html"
  , blogDefContext = baseContext
  , indexIdentifier = "drafts.html"
  , indexTemplate = "templates/drafts/index.html"
  , tagsPageTemplate = "templates/drafts/tag.html"
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
  isQuickPreview <- isTrue <$> lookupEnv "PREVIEW"
  let opts = def { quickPreview = isQuickPreview }
  copyHtAccess
  runHakyll opts
  where
    isTrue v = v `elem` map Just ["true", "True"]

--------------------------------------------------------------------------------