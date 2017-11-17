module DM.Coursework where

import Text.LaTeX.Packages.AMSMath (math, mathDisplay)

import ReportBase
import DM.Logic
import DM.CourseworkTruthTable
import DM.QuineMcCluskey

writeReport :: IO ()
writeReport = renderFile "./renders/DM-Coursework.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  defineWidebar
  usepackage [] "longtable"
  document $ do
    -- TODO: change the variant to 59
    baseTitlePage ("Курсовая работа", "Дискретная математика", Just "Вариант 60", "2017 г.")
    sectionstar "Функция"
    mt "f=1" <> ", когда истинно " <> mt "9 < (1x_4x_5 + x_1x_2x_3) \\leqslant 12" >> parbreak
    mt "f=d" <> ", когда истинно " <> mt "|x_5x_1x_2 - x_4x_3| = 0, 5"
    sectionstar "Таблица истинности функции"
    truthTableTeX
    sectionstar "Представление функции в аналитическом виде"
    "КДНФ: " <> (math . rendertex . sumOfProducts) truthTable <> parbreak
    "ККНФ: " <> (math . rendertex . productOfSums) truthTable <> parbreak
    newpage
    sectionstar "Нахождение минимальных форм методом Квайна--Мак-Класки"
    implicantTable
