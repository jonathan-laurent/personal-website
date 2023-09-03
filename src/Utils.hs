{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( baseContext
    , createdFirst
    , alphabetical
    , revAlphabetical
    , useAbsoluteUrls
    , forItem
    , depField
    , monadicField
    , yamlMetadataField
    , yamlField
    , rawYamlField
    , getBoolMetadata
    , enableMarkdown
    , markdownFunctionField
    , inlineMarkdownFunctionField
    , beautifyHtml ) where

--------------------------------------------------------------------------------

import Hakyll
import Filters.Hidden (walkHidden)

import Data.Monoid
import Control.Monad
import Text.Pandoc

import Data.List (sortBy, isPrefixOf)
import Data.Ord (comparing)
import Text.Pandoc.Readers (readMarkdown)
import Text.Pandoc.Writers (writeHtml5String)
import Data.Text (pack, unpack)

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Yaml as Yaml
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Time as Time
import qualified Data.Scientific as Sci

--------------------------------------------------------------------------------

generationDateCtx :: Context a
generationDateCtx =
  field "gendate" (const genDate)
  where
    genDate = do
      time <- unsafeCompiler $ Time.getCurrentTime >>= Time.utcToLocalZonedTime
      return (Time.formatTime Time.defaultTimeLocale "%B %e, %Y at %H:%M" time)

itemIdCtx :: Context a
itemIdCtx =
  field "item-id" getId
  where
    getId item = return (map sanitize . toFilePath . itemIdentifier $ item)
    sanitize c = if c == '/' then ':' else c

formatDateFunctionField :: Context a
formatDateFunctionField =
  functionField "format-date" fmt
  where
    errorMsg = "The $format-date$ function should receive two arguments"
    fmt args _item =
      case args of
        [   ] -> error errorMsg
        [ _ ] -> error errorMsg
        _ : _ : _ : _ : _ -> error errorMsg
        [timeStr, format] -> do
          let locale = Time.defaultTimeLocale
          time :: Time.UTCTime <- Time.parseTimeM True locale "%F" timeStr
          return (Time.formatTime locale format time)


altContext :: Context a
altContext =
  generationDateCtx <>
  itemIdCtx <>
  markdownFunctionField <>
  inlineMarkdownFunctionField <>
  formatDateFunctionField

baseContext :: Context String
baseContext = altContext <> defaultContext

--------------------------------------------------------------------------------

pandocStr :: String -> String
pandocStr str = handleError . runPure $
  readMarkdown opts (pack str)
    >>= return . walkHidden
    >>= writeHtml5String def
    >>= return . unpack
  where
    handleError (Right x) = x
    handleError (Left err) = error (show err)
    opts = def { readerExtensions = pandocExtensions }


removePrefix :: String -> String -> String
removePrefix pre s =
  if pre `isPrefixOf` s then drop (length pre) s
  else s

removeSuffix :: String -> String -> String
removeSuffix suf s = reverse (removePrefix (reverse suf) (reverse s))

markdownFunctionField' :: String -> Bool -> Context a
markdownFunctionField' name stripEnclosingMarkup =
  functionField name markdown
  where
    markdown args _item =
      case args of
        []    -> error ("Missing argument for $" ++ name ++ "$")
        _:_:_ -> error ("Too many arguments for $" ++ name ++ "$")
        [arg] ->
          if stripEnclosingMarkup then
            return (stripEnclosing (pandocStr arg))
          else
            return (pandocStr arg)

    stripEnclosing = removePrefix "<p>" . removeSuffix "</p>"

    {- For some reason, Haskell regular expressions do not work well
       with Unicode

    stripEnclosing :: String -> String
    stripEnclosing s =
      let pat = "^<p>(.*)</p>$" :: String in
      case s =~ pat :: [[String]] of
        [] -> s
        [[_, sub]] -> traceShow (s =~ pat :: [[String]]) sub
        _ -> undefined
    -}


markdownFunctionField :: Context a
markdownFunctionField = markdownFunctionField' "markdown" False

inlineMarkdownFunctionField :: Context a
inlineMarkdownFunctionField = markdownFunctionField' "inline-markdown" True


stringFilter :: (String -> String) -> Context a -> Context a
stringFilter f ctx =
  Context $ \k args it -> process <$> unContext ctx k args it
  where
    process (StringField str) = StringField (f str)
    process (ListField c its) = ListField (stringFilter f c) its

enableMarkdown :: Context a -> Context a
enableMarkdown = stringFilter pandocStr

--------------------------------------------------------------------------------

createdFirst :: [Item String] -> Compiler [Item String]
createdFirst items = do
  itemsWithTime <- forM items $ \item -> do
    -- getItemUTC looks for the metadata "published" or "date"
    utcTime <- getItemUTC Time.defaultTimeLocale $ itemIdentifier item
    return (utcTime ,item)
  return $ map snd $ sortBy (flip (comparing fst)) itemsWithTime

alphabetical :: [Item String] -> Compiler [Item String]
alphabetical items =
  return $ sortBy (comparing itemIdentifier) items

revAlphabetical :: [Item String] -> Compiler [Item String]
revAlphabetical items =
  return $ sortBy (flip (comparing itemIdentifier)) items

--------------------------------------------------------------------------------

useAbsoluteUrls :: Item String -> Compiler (Item String)
useAbsoluteUrls = withItemBody $ \page -> do
  root <- loadBody "siteroot"
  return (withUrls (absolute root) page)
  where absolute root url =
          if "/" `isPrefixOf` url then root ++ url else url

--------------------------------------------------------------------------------

forItem :: Context a -> Compiler (Item a) -> Context b
ctx `forItem` it =
  Context $ \k args _it ->
    it >>= unContext ctx k args

depField :: (Item a -> Context a) -> Context a
depField f =
  Context $ \k args it ->
    unContext (f it) k args it

monadicField :: Compiler (Context a) -> Context a
monadicField cc =
  Context $ \k args it -> do
    c <- cc
    unContext c k args it

--------------------------------------------------------------------------------

yamlConstField :: String -> Item Yaml.Value -> Context a
yamlConstField name it =
 case itemBody it of
    Yaml.Null -> constField name ""
    Yaml.Bool b -> constField name (show b)
    Yaml.String s -> constField name (unpack s)
    Yaml.Number n -> constField name (numToString n)
    Yaml.Array a ->
      let children = map (`itemSetBody` it) (Vector.toList a) in
      let ctx = yamlContext itemFieldName <> altContext in
      listField name ctx (return children)
    Yaml.Object obj ->
      mconcat
        [ yamlConstField (unpack k) (itemSetBody v it)
        | (k, v) <- HashMap.toList obj ]
  where
    itemFieldName = "item"
    numToString n
      | Sci.isInteger n = Sci.formatScientific Sci.Fixed (Just 0) n
      | otherwise = show n

yamlContext :: String -> Context Yaml.Value
yamlContext name = depField (yamlConstField name)

yamlMetadataField :: Context a
yamlMetadataField =
  yamlContext "" `forItem`
    (getUnderlying >>= getMetadata >>= return . Yaml.Object >>= makeItem)

yamlField :: String -> Compiler (Item Yaml.Value) -> Context a
yamlField name it = yamlContext name `forItem` it

rawYamlField :: String -> Compiler (Item String) -> Context a
rawYamlField name cit = yamlField name yaml
  where
    yaml = do
      it <- cit
      case Yaml.decodeEither (UTF8.fromString (itemBody it)) of
        Left err -> error ("Parse error on '" ++ show (itemIdentifier it) ++ "': " ++ err)
        Right v -> return (itemSetBody v it)

--------------------------------------------------------------------------------

getBoolMetadata :: String -> Bool -> Compiler Bool
getBoolMetadata field def = do
  it <- getUnderlying
  m <- getMetadata it
  case lookupString field m of
    Nothing -> return def
    Just b ->
      if b `elem` ["yes", "true"] then return True
      else if b `elem` ["no", "false"] then return False
      else error ("Unrecognized boolean '" ++ b ++ "'.")

--------------------------------------------------------------------------------

beautifyHtml :: Item String -> Compiler (Item String)
-- beautifyHtml = withItemBody (unixFilter "tidy" ["-i"])
beautifyHtml = return

--------------------------------------------------------------------------------