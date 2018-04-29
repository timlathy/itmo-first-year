module LaTeXHelpers where

import Data.List (intersperse, findIndices)
import Text.LaTeX hiding (titlepage)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Base.Render (renderAppend)

setmainfont :: LaTeXC l => [l] -> String -> l
setmainfont opts name = liftListL (\ls_ -> TeXComm "setmainfont" [MOptArg ls_ ,FixArg $ fromString name]) opts

setmathfont :: LaTeXC l => [l] -> String -> l
setmathfont opts name = liftListL (\ls_ -> TeXComm "setmathfont" [MOptArg ls_ ,FixArg $ fromString name]) opts

setmonofont :: LaTeXC l => [l] -> String -> l
setmonofont opts name = liftListL (\ls_ -> TeXComm "setmonofont" [MOptArg ls_ ,FixArg $ fromString name]) opts

mt :: LaTeXC l => Text -> l
mt = math . raw

lnbreak :: Measure -> LaTeXM ()
lnbreak = (lnbk >>) . vspace

parbreak :: LaTeXM ()
parbreak = raw "\n\n"

sectionstar :: LaTeXC l => l -> l
sectionstar = comm1 "section*"

sssectionstar :: LaTeXC l => l -> l
sssectionstar = comm1 "subsubsection*"

flalignstar :: LaTeXC l => l -> l
flalignstar = environment "flalign*"

alignstar :: LaTeXC l => l -> l
alignstar = environment "align*"

landscapemode :: LaTeXC l => l -> l
landscapemode = environment "landscape"

centerbox :: LaTeXC l => l -> l
centerbox = environment2 "adjustbox" [FixArg "center"]

environment :: LaTeXC l => String -> l -> l
environment name = environment2 name []

environment2 :: LaTeXC l => String -> [TeXArg] -> l -> l
environment2 name = liftL . (TeXEnv name)

trow :: LaTeXC l => [l] -> l
trow cols = tfreerow cols <> hline

tfreerow :: LaTeXC l => [l] -> l
tfreerow cols = mconcat (intersperse (raw "&") cols) <> lnbk

borderedtable :: LaTeXC l => [(TableSpec, Int)] -> l -> l
borderedtable spec = tabular Nothing (expandspec spec)

expandspec :: [(TableSpec, Int)] -> [TableSpec]
expandspec = addBorders . expand
  where
    expand = ((\(s, times) -> replicate times s) =<<)
    addBorders = (flip (++) $ [VerticalLine]) . ((:) VerticalLine) . (intersperse VerticalLine)

multirow :: LaTeXC l => Int -> Maybe Measure -> l -> l
multirow colspan widthm = liftL $ \t -> TeXComm "multirow" [ FixArg $ TeXRaw $ render colspan, FixArg $ TeXRaw width, FixArg t ]
  where
    width = case widthm of
      Just m -> render m
      Nothing -> "*"

-- Source: https://tex.stackexchange.com/a/364929
defineWidebar :: LaTeXM ()
defineWidebar = raw "\\DeclareFontFamily{U}{mathx}{\\hyphenchar\\font45}\
                    \\\DeclareFontShape{U}{mathx}{m}{n}{ <-> mathx10 }{}\
                    \\\DeclareSymbolFont{mathx}{U}{mathx}{m}{n}\
                    \\\DeclareFontSubstitution{U}{mathx}{m}{n}\
                    \\\DeclareMathAccent{\\widebar}{\\mathalpha}{mathx}{\"73}\
                    \\\makeatletter\
                    \\\newcommand{\\cwidebar}[2][0]{{\\mathpalette\\@cwidebar{{#1}{#2}}}}\
                    \\\newcommand{\\@cwidebar}[2]{\\@cwideb@r{#1}#2}\
                    \\\newcommand{\\@cwideb@r}[3]{%\n\
                      \\\sbox\\z@{$\\m@th#1\\mkern-#2mu#3\\mkern#2mu$}%\n\
                      \\\widebar{\\box\\z@}%\n\
                    \}\
                    \\\makeatother"

-- Source: https://tex.stackexchange.com/a/12161
defineSqcases :: LaTeXM ()
defineSqcases = raw "\\makeatletter\
                    \\\newenvironment{sqcases}{%\n\
                    \\\matrix@check\\sqcases\\env@sqcases\
                    \}{%\n\
                    \\\endarray\\right.%\n\
                    \}\
                    \\\def\\env@sqcases{%\n\
                      \\\let\\@ifnextchar\\new@ifnextchar\n\
                      \\\left\\lbrack\n\
                      \\\def\\arraystretch{1.2}%\n\
                      \\\array{@{}l@{\\quad}l@{}}%\n\
                    \}\
                    \\\makeatother"

