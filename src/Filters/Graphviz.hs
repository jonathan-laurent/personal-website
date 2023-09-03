{-# LANGUAGE OverloadedStrings #-}

module Filters.Graphviz (processGraphviz) where

import Filters.Common

graphviz :: Block -> AttachedT Compiler Block
graphviz (CodeBlock (id, "graph":cls, attrs) code) = do
  let template = "scripts/templates/graph.dot"
  graphviz <- lift (applyExtTemplate template mempty code)
  file <- attachCmd "graph.svg" "dot -Tsvg > %" graphviz
  let html = htmlMarkup "img" (id, "graphviz" : cls, ("src", file) : attrs) ""
  return (RawBlock (Format "html") html)
graphviz x = return x

processGraphviz :: Filter (AttachedT Compiler)
processGraphviz = makeFilter (walkM graphviz)