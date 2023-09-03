{-# LANGUAGE OverloadedStrings #-}

module Page
    ( pageCompiler
    , simpleMarkdownPageCompiler
    ) where

import Hakyll
import Config
import Utils
import Attached
import Compilers

import Data.Monoid
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension, (</>))
import Control.Monad.Trans.Class (lift)
import Control.Applicative (empty, (<|>))

import Data.Yaml
import qualified Data.Text as Text

--------------------------------------------------------------------------------

simpleMarkdownPageCompiler :: SiteOptions -> Compiler (Item (Attached String))
simpleMarkdownPageCompiler opts =
  withAttachedFiles $ do
    pandoc <- markdownCompiler opts
    lift (
      loadAndApplyTemplate "templates/page-simple.html" baseContext pandoc
      >>= loadAndApplyTemplate baseTemplate baseContext
      >>= relativizeUrls )

--------------------------------------------------------------------------------

data ItemsOrder = Alphabetical | RevAlphabetical | Date

readOrder :: String -> Maybe ItemsOrder
readOrder s
    | s `elem` ["alpha", "alphabetical"] = Just Alphabetical
    | s `elem` ["revalpha", "rev-alphabetical"] = Just RevAlphabetical
    | s `elem` ["date", "oldest-first"] = Just Date
    | otherwise = Nothing

data DataSource =
      FolderSource Pattern ItemsOrder
    | YamlSource Identifier
    | TextSource Identifier
    | MultipleSources [Identifier]

data DataField = DataField { fieldName :: String , src :: DataSource }

newtype DataFields = DataFields [DataField]


instance FromJSON DataField where
  parseJSON = withObject "DataField" $ \v ->
    DataField <$> (Text.unpack <$> v .: "field")
              <*> ((multipleSources <$> v .: "src")
              <|> (parseSource v =<< v .: "src"))
    where

      parseOrder :: Maybe String -> ItemsOrder
      parseOrder = fromMaybe RevAlphabetical . (>>=readOrder)

      multipleSources :: [String] -> DataSource
      multipleSources ids = MultipleSources (map fromFilePath ids)

      parseSource :: Object -> String -> Parser DataSource
      parseSource v s =
        let ext = takeExtension s in
        if ext == "" then do
          ord <- parseOrder <$> (v .:? "order")
          return (FolderSource (fromGlob (s </> "*.md")) ord)
        else if ext == ".yaml" then
          return (YamlSource (fromFilePath s))
        else if ext `elem` [".md", ".txt"] then
          return (TextSource (fromFilePath s))
        else
          error ("Unsupported source format: '" ++ s ++ "'")


instance FromJSON DataFields where
  parseJSON = withObject "DataFields" $ \v -> DataFields <$> v .: "data"

compileItemsOrder :: ItemsOrder -> [Item String] -> Compiler [Item String]
compileItemsOrder Alphabetical = alphabetical
compileItemsOrder RevAlphabetical = revAlphabetical
compileItemsOrder Date = createdFirst

compileDataField :: DataField -> Context a
compileDataField df =
  case src df of
    YamlSource id -> rawYamlField name (load id)
    TextSource id -> bodyField name `forItem` load id
    FolderSource pat order ->
      listField name baseContext (compileItemsOrder order =<< loadAll pat)
    MultipleSources ids ->
      listField name baseContext (mapM load ids)
    where name = fieldName df

compileDataFields :: DataFields -> Context a
compileDataFields (DataFields dfs) = mconcat (map compileDataField dfs)

dataContext :: Context a
dataContext = monadicField $ do
    it <- getUnderlying
    m <- getMetadata it
    case parseMaybe parseJSON (Object m) :: Maybe DataFields of
      Nothing -> empty
      Just dfs -> return (compileDataFields dfs)

--------------------------------------------------------------------------------

pageCompiler :: Compiler (Item String)
pageCompiler = do
  rel <- getBoolMetadata "relativize-urls" True
  getResourceBody
    >>= applyAsTemplate context
    >>= loadAndApplyTemplate baseTemplate baseContext
    >>= beautifyHtml
    >>= if rel then relativizeUrls else useAbsoluteUrls
  where
    context = dataContext <> baseContext

--------------------------------------------------------------------------------