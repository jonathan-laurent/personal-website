{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AsciiMath.Lexer (Token(..), lex) where

import Prelude hiding (lex)
import Text.Parsec hiding (tokens, token)
import Control.Monad
import Data.Char hiding (Space)
import qualified Data.Map.Strict as Map

import AsciiMath.Spec

-------------------------------------------------------------------------------

data Token =
  Lit String
  | Special String
  | Binop String
  | String String
  | Del String
  | DelO String
  | DelC String
  | Fun String Arity
  | Space Int
  | ParO             -- (
  | ParC             -- )
  | Underscore       -- _
  | Hat              -- \^
  deriving (Show, Eq)

specialTokens =
  [ ("(",  ParO)
  ,  (")",  ParC)
  ,  ("_",  Underscore)
  ,  ("^",  Hat) 
  ]

-------------------------------------------------------------------------------

tokChar = satisfy admissible
  where admissible c = not (isSpace c || c == '"')

otherBlank = many (satisfy (\c -> isSpace c && c /= ' '))

semanticSpace = (Space . length) <$> many1 (char ' ')

tokens spec = filter keep <$> many (otherBlank *> token spec <* otherBlank)
  where keep (Space n) = n >= 2
        keep _ = True

token spec = semanticSpace <|> string <|> matchLen keyMaxLen
  where
    dic = Map.fromList $
      specialTokens ++
      [(s,  Special f) | (s, f) <- aliases spec] ++
      [(f,  Fun f ar)  | (f, ar) <- functions spec] ++
      [(op, DelO f)    | (op, cl, f) <- delimiters spec, op /= cl ] ++
      [(cl, DelC f)    | (op, cl, f) <- delimiters spec, op /= cl ] ++
      [(op, Del  op)   | (op, cl, _f) <- delimiters spec, op == cl ] ++
      [(s,  Binop f)   | (s, f, _) <- binops spec]

    keyMaxLen = maximum (map length (Map.keys dic))

    takeLen l = do
      w <- count l tokChar
      case Map.lookup w dic of
        Nothing  -> mzero
        Just tok -> return tok

    matchLen 0 = fmap Lit (count 1 tokChar)
    matchLen l = try (takeLen l) <|> matchLen (l-1)

    string = char '"' *> (String <$> many (satisfy (/='"'))) <* char '"'


lex :: Spec -> String -> Either ParseError [Token]
lex spec = parse (tokens spec) ""

-------------------------------------------------------------------------------