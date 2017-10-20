module LaTeXHelpers where

import Data.List (intersperse, findIndices)
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

alignstar :: LaTeXC l => l -> l
alignstar = environment "align*"

landscapemode :: LaTeXC l => l -> l
landscapemode = environment "landscape"

environment :: LaTeXC l => String -> l -> l
environment name = liftL $ TeXEnv name []

trow :: LaTeXC l => [l] -> l
trow cols = tfreerow cols <> hline

tfreerow :: LaTeXC l => [l] -> l
tfreerow cols = mconcat (intersperse (raw "&") cols) <> lnbk

multirow :: LaTeXC l => Int -> Maybe Measure -> l -> l
multirow colspan widthm = liftL $ \t -> TeXComm "multirow" [ FixArg $ TeXRaw $ render colspan, FixArg $ TeXRaw width, FixArg t ]
  where
    width = case widthm of
      Just m -> render m
      Nothing -> "*"
