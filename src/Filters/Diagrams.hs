{-# LANGUAGE OverloadedStrings #-}

module Filters.Diagrams (processDiagrams) where

import Filters.Common

diag :: Block -> AttachedT Compiler Block
diag (CodeBlock (id, "diag":cls, attrs) d) = do
  let template = "scripts/templates/diag.hs"
  let ctx = contextFromAttributesIn ["height", "width"] attrs
  haskellCode <- lift (applyExtTemplate template ctx d)
  let cmd = "scripts/diagrams.sh %"
  file <- attachCmd "diag.svg" cmd haskellCode
  let html = htmlMarkup "img" (id, "diagram" : cls, ("src", file) : attrs) ""
  return (RawBlock (Format "html") html)
diag x = return x

processDiagrams :: Filter (AttachedT Compiler)
processDiagrams = makeFilter (walkM diag)