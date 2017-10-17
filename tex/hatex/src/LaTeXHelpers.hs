module LaTeXHelpers where

import Text.LaTeX hiding (titlepage)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

setmainfont :: LaTeXC l => [l] -> String -> l
setmainfont opts name = liftListL (\ls_ -> TeXComm "setmainfont" [MOptArg ls_ ,FixArg $ fromString name]) opts

setmonofont :: LaTeXC l => [l] -> String -> l
setmonofont opts name = liftListL (\ls_ -> TeXComm "setmonofont" [MOptArg ls_ ,FixArg $ fromString name]) opts

lnbreak :: Measure -> LaTeXM ()
lnbreak = (lnbk >>) . vspace

parbreak :: LaTeXM ()
parbreak = raw "\n\n"

sectionstar :: LaTeXC l => l -> l
sectionstar = comm1 "section*"

flalignstar :: LaTeXC l => l -> l
flalignstar = environment "flalign*"

landscapemode :: LaTeXC l => l -> l
landscapemode = environment "landscape"

environment :: LaTeXC l => String -> l -> l
environment name = liftL $ TeXEnv name []
