module Config where

import Hakyll
import Data.Default

--------------------------------------------------------------------------------

attachmentDirSuffix :: String
attachmentDirSuffix = "-data"

baseTemplate :: Identifier
baseTemplate = fromFilePath "templates/base.html"

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { deployCommand = "./deploy.sh"
  , providerDirectory = "provider"
  , destinationDirectory = "generated/deploy/out"
  , storeDirectory = "generated/deploy/cache"
  , tmpDirectory = "generated/deploy/cache/tmp"
  , previewHost = "0.0.0.0"
  , previewPort = 4000
  }

--------------------------------------------------------------------------------

newtype PostOptions = PostOptions
  { mathModules :: [String] } deriving Show

instance Default PostOptions where
  def = PostOptions { mathModules = [] }

postOptions :: Compiler PostOptions
postOptions = do
  metadata <- getUnderlying >>= getMetadata
  let modules = maybe [] parseList $ lookupString "math" metadata
  return (PostOptions modules)
  where parseList = map trim . splitAll ","

--------------------------------------------------------------------------------

data SiteOptions = SiteOptions
  { quickPreview :: Bool
  , pygmentStyle :: String }

instance Default SiteOptions where
  def = SiteOptions
    { quickPreview = False 
    , pygmentStyle = "trac" }

--------------------------------------------------------------------------------