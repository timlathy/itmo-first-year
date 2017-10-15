module Inf.Lab3 where

import ReportBase

writeReport :: IO ()
writeReport = renderFile "./renders/Inf-Lab3.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  document $ do
    -- TODO: change the subject
    baseTitlePage ("ЛАБОРАТОРНАЯ РАБОТА №3", "Предмет", "2017 г.")
    -- ...
