module Filters.Code (processCode, createPygmentStyle) where

import Filters.Common

--------------------------------------------------------------------------------

lexer :: String -> String
lexer "cs" = "csharp"
lexer str = str

codeMarkup :: String -> String
codeMarkup = htmlMarkup "code" ("", [], [])

pygTrans :: Block -> Block
pygTrans (CodeBlock (id, [lang], attrs) code) =
    let inner = runPygment (lexer lang) code in
    let html = htmlMarkup "div" (id, ["listing"], attrs) (codeMarkup inner) in
    RawBlock (Format "html") html
pygTrans x = x

runPygment :: String -> String -> String
runPygment lang txt = unsafePerformIO $
  if lang `elem` ["kappa", "katql"] then
    let script = "scripts/" ++ lang ++ "_pygment.py" in
    readProcess "python" [script] escapedText
  else
    readProcess "pygmentize" ["-l", lang, "-f", "html"] escapedText
    `catch` handler
  where
    handler :: IOError -> IO String
    handler _e = return (htmlMarkup "pre" ("", [], []) escapedText)
    escapedText = escapeHtmlPre txt


processCode :: Monad m => Filter m
processCode = makePureFilter (walk pygTrans)

createPygmentStyle :: Identifier -> String -> Rules ()
createPygmentStyle file style =
  create [file] $ do
    route idRoute
    compile $
      unixFilter "pygmentize" ["-S", style, "-f", "html"] ""
      >>= makeItem

--------------------------------------------------------------------------------