module Filters.Math (MathConfig(..), processMath) where

import AsciiMath
import Filters.Common
import Text.TeXMath.Readers.TeX.Macros as Macros

--------------------------------------------------------------------------------

data MathConfig = MathConfig
  { texMacros     :: [Macro]
  , asciiMathSpec :: AsciiMath.Spec }

convert :: MathConfig -> String -> String
convert cfg str =
  applyMacros
      (reverse (texMacros cfg))
      (AsciiMath.translate (asciiMathSpec cfg) str)

convertInline :: MathConfig -> Inline -> Inline
convertInline cfg (Math t str) =
  Math t (convert cfg str)
convertInline _ x = x

convertAlign :: MathConfig -> Block -> Block
convertAlign cfg (CodeBlock (_id, ["align"], _attrs) str) =
  Para [Math DisplayMath (process . convert cfg $ str)]
  where process str = "\\begin{aligned} " ++ str ++ " \\end{aligned}"
convertAlign _ x = x

processMath :: Monad m => MathConfig -> Filter m
processMath cfg = makePureFilter $
  walk (convertAlign cfg) . walk (convertInline cfg)

--------------------------------------------------------------------------------

-- Old approach: let AJAX do the macro processing (does not work with KaTex)
_injectLatexMacros :: String -> Pandoc -> Pandoc
_injectLatexMacros macros p =
  let block =
        Div ("",[],[("style","display:none")]) [Para [Math DisplayMath macros]]
  in Pandoc nullMeta [block] <> p

--------------------------------------------------------------------------------
