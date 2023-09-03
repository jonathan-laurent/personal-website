{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AsciiMath.Spec (
  Arity,
  Precedence,
  FunRef,
  Spec(..),
  specParser,
  readSpec,
  specFromFile) where

import Text.Parsec hiding (spaces)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------

type Arity      = Int
type Precedence = Int    -- Range: 1 (lowest) to 10 (highest)
type FunRef     = String

data Spec = Spec
  { functions  :: [(FunRef, Arity)]
  , delimiters :: [(String, String, FunRef)]
  , aliases    :: [(String, FunRef)]
  , binops     :: [(String, FunRef, Precedence)]
  } deriving (Show)

instance Semigroup Spec where
  s <> s' = Spec
    { functions  = functions s  ++ functions s',
      delimiters = delimiters s ++ delimiters s',
      aliases    = aliases s    ++ aliases s',
      binops     = binops s     ++ binops s' }

instance Monoid Spec where
  mempty = Spec { functions = [], delimiters = [], aliases = [], binops = [] }

-------------------------------------------------------------------------------

spaces = many (oneOf " \t")

word = many1 (satisfy (not . isSpace)) <* spaces

header s = string s <* spaces

number = many1 digit <* spaces

numberDef def = (read . fromMaybe def) <$> optionMaybe number

funParser = do
  header "fun"
  name  <- word
  arity <- numberDef "1"
  return mempty {functions=[(name, arity)]}

texParser = do
  header "tex"
  specTex <$> many word
  where
    specTex xs = mconcat [mempty {aliases=[(x, x)]} | x <- xs]

aliasParser = do
  header "alias"
  specAlias <$> word <*> word
  where
    specAlias s funref = mempty {aliases=[(s, funref)]}

opParser = do
  header "op"
  specOp <$> word <*> word <*> numberDef "5"
  where
    specOp s fun prec = mempty {binops=[(s, fun, prec)]}

delParser = do
  header "del"
  open <- word
  close <- word
  fun <- word
  return mempty {delimiters=[(open, close, fun)]}

nullLineParser = do
  comment <|> spaces
  return mempty
  where
    comment = do
      char '#'
      many (noneOf "\n")

-------------------------------------------------------------------------------

specParser :: Parsec String st Spec
specParser = mconcat <$> (instr `sepEndBy` many1 endOfLine)
  where instr = funParser <|> texParser <|> aliasParser <|> opParser <|> delParser <|> nullLineParser

readSpec :: String -> Spec
readSpec str =
  case parse specParser "" str of
    Right spec -> spec
    Left _ -> error "Invalid math configuration file."

specFromFile :: FilePath -> IO Spec
specFromFile f = handleError . parse specParser f <$> readFile f
  where handleError (Right spec) = spec
        handleError (Left _) = mempty

-------------------------------------------------------------------------------