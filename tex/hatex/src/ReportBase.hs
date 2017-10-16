module ReportBase
  ( module ReportBase
  , module Text.LaTeX ) where

import Text.LaTeX hiding (titlepage)
import Text.LaTeX.Base.Class

import Text.LaTeX.Base.Syntax

import qualified Identity.Student as Student
import qualified Identity.Institution as Institution

baseHeader :: LaTeXM ()
baseHeader = do
  documentclass [a4paper, "12pt"] report
  usepackage [ "a4paper", "mag=1000"
             , "left=1.5cm", "right=1.5cm", "top=1.5cm", "bottom=1.5cm"
             , "headsep=0.7cm", "footskip=1cm" ] "geometry"
  -- Always indent the first paragraph
  usepackage [] "indentfirst"
  -- Fonts
  usepackage [] "fontspec, unicode-math"
  setmainfont ["Ligatures=TeX"] "CMU Serif"
  setmonofont [] "CMU Typewriter Text"
  -- Proper quotes
  usepackage [] "upquote"
  -- Math alignment
  usepackage ["fleqn"] "amsmath"
  -- Trees
  usepackage [] "qtree"
  -- Landscape orientation env
  usepackage [] "pdflscape"
  usepackage [] "tikz"

baseTitlePage :: (LaTeXM (), LaTeXM (), LaTeXM ()) -> LaTeXM ()
baseTitlePage (reportTitle, reportSubject, reportYear) =
  titlepageEnv $ do
      center $ do
        textsc (Institution.name >> lnbreak (Mm 4) >> Institution.department)
        vfill
        textbf (reportTitle >> lnbreak (Mm 2) >> reportSubject) >> lnbreak (Mm 20)
        Student.name >> lnbreak (Mm 2) >> Student.group
        vfill
        Institution.location >> lnbreak (Mm 2) >> reportYear
  where
    titlepageEnv = liftL $ TeXEnv "titlepage" []

setmainfont :: LaTeXC l => [l] -> String -> l
setmainfont opts name = liftListL (\ls_ -> TeXComm "setmainfont" [MOptArg ls_ ,FixArg $ fromString name]) opts

setmonofont :: LaTeXC l => [l] -> String -> l
setmonofont opts name = liftListL (\ls_ -> TeXComm "setmonofont" [MOptArg ls_ ,FixArg $ fromString name]) opts

lnbreak :: Measure -> LaTeXM ()
lnbreak = (lnbk >>) . vspace

sectionstar :: LaTeXC l => l -> l
sectionstar = comm1 "section*"

flalignstar :: LaTeXC l => l -> l
flalignstar = liftL $ TeXEnv "flalign*" []

landscapemode :: LaTeXC l => l -> l
landscapemode = liftL $ TeXEnv "landscape" []
