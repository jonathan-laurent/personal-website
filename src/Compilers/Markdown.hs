{-# LANGUAGE OverloadedStrings #-}

module Compilers.Markdown (markdownCompiler) where

import Hakyll
import Config
import Text.Pandoc.Options
import Text.TeXMath.Readers.TeX.Macros (parseMacroDefinitions)

import Data.String (fromString)
import Control.Monad.Trans.Class

import Attached
import Filters
import AsciiMath (readSpec)

--------------------------------------------------------------------------------

mathConfig :: PostOptions -> Compiler MathConfig
mathConfig opts = do
  macros <- mconcat <$> mapM loadMacro modules
  spec   <- mconcat <$> mapM loadSpec modules
  return (MathConfig macros spec)
  where
    modules = "default" : mathModules opts

    loadMacro mod =
      fst . parseMacroDefinitions <$>
      loadBody (fromString $ "math/" ++ mod ++ "-macros.tex")

    loadSpec mod =
      AsciiMath.readSpec <$>
      loadBody (fromString $ "math/" ++ mod ++ ".conf")


markdownCompiler :: SiteOptions -> AttachedT Compiler (Item String)
markdownCompiler opts = do
  options    <- lift postOptions
  mathConfig <- lift (mathConfig options)
  
  let rOpts = defaultHakyllReaderOptions
  let wOpts = defaultHakyllWriterOptions
        { writerHTMLMathMethod = MathJax "" }
  
  writePandocWith wOpts <$> (
    lift (getResourceBody >>= readPandocWith rOpts)
    >>= processMath mathConfig
    >>= processGraphviz
    >>= processDiagrams
    >>= (if quickPreview opts then return else processCode))

  --------------------------------------------------------------------------------