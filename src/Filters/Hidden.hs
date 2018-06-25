module Filters.Hidden (processHidden, walkHidden) where

import Filters.Common

--------------------------------------------------------------------------------

processButton :: Inline -> Inline
processButton (Span (id, classes, args) body)
    | Just block <- lookup "toggle" args =
        let script = "toggle(\"" ++ block ++ "\")"
            newArgs = ("onclick", script) : filter ((/= "toggle").fst) args in
        Span (id, "toggle" : classes, newArgs) body
processButton x = x

processBlock :: Block -> Block
processBlock (Div (id, classes, args) body) | "hidden" `elem` classes =
    let style = "display:none" in
    Div (id, classes, ("style", style) : args) body
processBlock b = b

walkHidden :: Pandoc -> Pandoc
walkHidden = walk processButton . walk processBlock

processHidden :: Filter (AttachedT Compiler)
processHidden = makePureFilter walkHidden

{-------------------------------------------------------------------------------

Blocks that can be shown and hidden.

    [Click here]{toggle="block-id"}

    ::: { #block-id .hidden } :::

    :::

is turned into:

    <span onclick='toggle("block-id")'> Click here </span>
    <div id="block-id" style="display:none"> ... </div>

-------------------------------------------------------------------------------}