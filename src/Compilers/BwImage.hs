{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compilers.BwImage (bwImageCompiler) where

import Hakyll

import Data.Binary
import Data.Typeable
import System.FilePath
import System.Process (callCommand)
import Config (hakyllConfig)

newtype ConvertImage = ConvertImage FilePath 
    deriving (Binary, Eq, Ord, Show, Typeable)

instance Writable ConvertImage where
    write dst (Item _ (ConvertImage src)) = 
        callCommand ("convert " ++ src ++ " -colorspace Gray " ++ dst)

bwImageCompiler :: Compiler (Item ConvertImage)
bwImageCompiler = do
    file <- toFilePath <$> getUnderlying
    makeItem $ ConvertImage $ providerDirectory hakyllConfig </> file