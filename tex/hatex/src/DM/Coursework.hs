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
    baseTitlePage ("Курсовая работа", "Дискретная математика", Just "Вариант 59", "2017 г.")
    sectionstar "Функция"
    mt "f=1" <> ", когда истинно " <> mt "3 \\leqslant (x_1x_2x_3+x_4x_5) < 7" >> parbreak
    mt "f=d" <> ", когда истинно " <> mt "(x_3x_4) = 2"
    sectionstar "Таблица истинности функции"
    truthTableTeX
    sectionstar "Представление функции в аналитическом виде"
    "КДНФ: " <> (math . rendertex . sumOfProducts) truthTable <> parbreak
    "ККНФ: " <> (math . rendertex . productOfSums) truthTable <> parbreak
    newpage
    sectionstar "Нахождение минимальных форм методом Квайна--Мак-Класки"
    implicantTable
