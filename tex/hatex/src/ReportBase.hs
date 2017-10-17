module ReportBase
  ( module ReportBase
  , module LaTeXHelpers
  , module Text.LaTeX ) where

import Text.LaTeX hiding (titlepage)

import LaTeXHelpers

import qualified Identity.Student as Student
import qualified Identity.Institution as Institution

baseHeader :: LaTeXM ()
baseHeader = do
  documentclass [a4paper, "12pt"] report
  usepackage [ "a4paper", "mag=1000"
             , "left=1.5cm", "right=1.5cm", "top=1.5cm", "bottom=1.5cm"
             , "headsep=0.7cm", "footskip=1cm" ] "geometry"
  -- Fonts
  usepackage [] "fontspec, unicode-math"
  setmainfont ["Ligatures=TeX"] "CMU Serif"
  setmonofont [] "CMU Typewriter Text"
  -- Proper quotes
  usepackage [] "upquote"
  -- Math alignment
  usepackage ["fleqn"] "amsmath"
  -- tikz
  usepackage [] "tikz"
  -- Trees
  usepackage [] "qtree"
  -- Landscape orientation env
  usepackage [] "pdflscape"
   -- Always indent the first paragraph
  usepackage [] "indentfirst"

baseTitlePage :: (LaTeXM (), LaTeXM (), LaTeXM ()) -> LaTeXM ()
baseTitlePage (reportTitle, reportSubject, reportYear) =
  environment "titlepage" $ do
    center $ do
      textsc (Institution.name >> lnbreak (Mm 4) >> Institution.department)
      vfill
      textbf (reportTitle >> lnbreak (Mm 2) >> reportSubject) >> lnbreak (Mm 20)
      Student.name >> lnbreak (Mm 2) >> Student.group
      vfill
      Institution.location >> lnbreak (Mm 2) >> reportYear
