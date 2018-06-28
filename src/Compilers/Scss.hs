module Compilers.Scss (compressScssCompiler) where

import Hakyll

import Config (hakyllConfig)
import System.FilePath ((</>))

styleFolder :: FilePath
styleFolder = "css" </> "style"

compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
  -- This is important to get the dependencies right
  _ <- loadAll (fromGlob (styleFolder </> "**.scss")) :: Compiler [Item ()]
  fmap (fmap compressCss) $
    getResourceString
    >>= withItemBody (unixFilter "sass" opts)
  where
    loadPath = providerDirectory hakyllConfig </> styleFolder
    opts = [ "--stdin", "--style", "compressed", "--load-path", loadPath ]
