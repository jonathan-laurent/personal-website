{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AsciiMath.Render (renderAst, renderText) where

import AsciiMath.Ast

import Text.PrettyPrint
import Data.Char hiding (Space)

-------------------------------------------------------------------------------

underscore = text "_"
hat = text "^"
escape = text "\\"
texF f args = escape <> text f <> hcat (map braces args)

ast (Ast es) = hsep (map expr es)

expr (Sup (Sub e sub) sup) =
  expr e <> underscore <> braces (expr sub) <>
  hat <> braces (expr sup)
expr (Sub e s) = expr e <> underscore <> braces (expr s)
expr (Sup e s) = expr e <> hat <> braces (expr s)
expr (Text s) = latexText s

expr (Fun f args) = texF f (map expr args)
expr (Lit s) = text s
expr (Group a) = ast a
expr (Special s) =
  if all isAlphaNum s
  then escape <> text s
  else text s

expr (Space n)
  | n == 2 = text "\\,"
  | n == 3 = text "\\quad"
  | n >= 4 = text "\\qquad"
  | otherwise = mempty

latexText s = texF "text" [text s]

-------------------------------------------------------------------------------

renderAst :: Ast -> String
renderAst = render . ast

renderText :: String -> String
renderText = render . latexText

-------------------------------------------------------------------------------