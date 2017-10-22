module DM.Coursework where

import ReportBase
import DM.Logic
import DM.CourseworkTruthTable

writeReport :: IO ()
writeReport = renderFile "./renders/DM-Coursework.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  document $ do
    baseTitlePage ("Курсовая работа", "Дискретная математика", Just "Вариант 60", "2017 г.")
    sectionstar "Функция"
    mt "f=1" <> ", когда истинно " <> mt "9 < (1x_4x_5 + x_1x_2x_3) \\leqslant 12" >> parbreak
    mt "f=d" <> ", когда истинно " <> mt "|x_5x_1x_2 - x_4x_3| = 0, 5"
    sectionstar "Таблица истинности"
    truthTableTeX

