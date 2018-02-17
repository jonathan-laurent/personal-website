module Compilers.Scss (compressScssCompiler) where

import Hakyll

compressScssCompiler :: Compiler (Item String)
compressScssCompiler =
  fmap (fmap compressCss) $
    getResourceString
    >>= withItemBody (unixFilter "sass" opts)
  where opts = 
          [ "-s", "--scss", "--style", "compressed", "--load-path", "scss" ]