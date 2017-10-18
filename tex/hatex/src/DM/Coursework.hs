module DM.Coursework where

import ReportBase
import DM.Logic

writeReport :: IO ()
writeReport = renderFile "./renders/DM-Coursework.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  document $ do
    baseTitlePage ("Курсовая работа", "Дискретная математика", "2017 г.")
 
