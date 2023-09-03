module AsciiMath
  ( Spec
  , readSpec
  , specFromFile
  , translate
  ) where

import AsciiMath.Spec (Spec, specFromFile, readSpec)

import AsciiMath.Parse
import AsciiMath.Render

translate :: Spec -> String -> String
translate spec s =
  case parseAst spec s of
    Right ast -> renderAst ast
    Left err -> renderText ("Error: " ++ show err ++ "")