module DepC.Reader
  (
  ) where

import DepC.Syntax
import Language.CCS ()
import Language.CCS.Recognize (Recog, runRecogs)

readFile :: FileName -> [Mod]
readFile fname = 


