--------------------------------------------------------------------------------

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

import Data.Maybe
import System.Environment (lookupEnv)

type Diag = Diagram B

--------------------------------------------------------------------------------

diagSize :: V2 (Maybe Double)
$if(width)$
diagSize = Just $width$ ^& Nothing
$else$
$if(height)$
diagSize = Nothing ^& Just $height$
$else$
diagSize = Nothing ^& Nothing
$endif$
$endif$

--------------------------------------------------------------------------------

$body$
  
--------------------------------------------------------------------------------

main :: IO ()
main = do
  output <- lookupEnv "OUTPUT"
  let outputFile = 
        case output of
          Nothing  -> error "The OUTPUT environment variable is not set."
          Just out -> out
          
  renderSVG outputFile (mkSizeSpec diagSize) (pad 1.1 diag)

--------------------------------------------------------------------------------
