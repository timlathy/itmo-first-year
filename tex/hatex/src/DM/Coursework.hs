module DM.Coursework where

import Data.List (intersperse)
import Control.Arrow ((&&&))
import Control.Monad (forM_)
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
    sectionstar "Нахождение минимальных форм методом Квайна--Мак-Класки"
    "Множество 0-кубов функции, составленное из конъюнктивных термов КДНФ и конъюнктивных термов, на которых функция принимает значение d:" >> newline
    let zcubes = cubeDisplaySort . zeroCubes . (uncurry (++)) . (minterms &&& dontcareminterms) $ truthTable
    mathDisplay $ do
      raw "\\left\\{"
      raw "\\begin{array}{l}"
      mconcat . (intersperse lnbk) . (rendertex <$>) $ zcubes
      raw "\\end{array}"
      raw "\\right\\}"
