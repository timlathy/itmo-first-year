module DM.Coursework where

import Text.LaTeX.Packages.AMSMath (math)
import Text.LaTeX.Packages.Graphicx (IGOption (..), includegraphics)

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
    textit "Нахождение простых импликант (максимальных кубов):" <> lnbk <> parbreak
    cubeTable <> newpage
    textit "Составление импликантной таблицы:" <> lnbk <> parbreak
    implicantTable <> lnbk <> parbreak
    "Все импликанты, исключая " <> textbf "XX10X" <> ", являются существенными, так как покрывают вершины, не покрытые другими импликантами." <> lnbk <> parbreak
    mt "C_{min} = \\set{\\text{0X1XX, 01X1X, 01XX1, 0XX11, 1XX00, 10X0X, 100X0}}" <> lnbk <> parbreak
    mt "S^a = 2 + 3 + 3 + 3 + 3 + 3 + 4 = 21, \\quad S^b = 21 + 7 = 28" <> lnbk <> parbreak
    "МДНФ: " <> (math . rendertex $ Or [ And [Not (X 1), X 3]
                                       , And [Not (X 1), X 2, X 4]
                                       , And [Not (X 1), X 2, X 5]
                                       , And [Not (X 1), X 4, X 5]
                                       , And [X 1, Not (X 4), Not (X 5)]
                                       , And [X 1, Not (X 2), Not (X 4)]
                                       , And [X 1, Not (X 2), Not (X 3), Not (X 5)] ])
    newpage
    sectionstar "Минимизация булевой функции на картах Карно"
    includegraphics [IGWidth $ Cm 12] "../src/DM/KarnaughMap.pdf" <> lnbk <> parbreak
    mt "C_{min} = \\set{\\text{0X1XX, 01X1X, 01XX1, 0XX11, 10X0X, 1XX00, 100X0}}" <> lnbk <> parbreak
    mt "S^a = 2 + 3 + 3 + 3 + 3 + 3 + 4 = 21, \\quad S^b = 21 + 7 = 28" <> lnbk <> parbreak
    "МДНФ: " <> (math . rendertex $ Or [ And [Not (X 1), X 3]
                                       , And [Not (X 1), X 2, X 4]
                                       , And [Not (X 1), X 2, X 5]
                                       , And [Not (X 1), X 4, X 5]
                                       , And [X 1, Not (X 2), Not (X 4)]
                                       , And [X 1, Not (X 4), Not (X 5)]
                                       , And [X 1, Not (X 2), Not (X 3), Not (X 5)] ]) <> lnbk <> parbreak
    "Можно заметить, что в данном случае минимальное покрытие, полученное с помощью карт Карно, полностью совпадает с минимальным покрытием, полученным методом Квайна--Мак-Класки."
    newpage
    sectionstar "Определение МКНФ"
    "Для определения МКНФ используется карта Карно с клетками, соответствующими наборам, на которых функция принимает нулевое значение, обозначенными нулем." <> lnbk <> parbreak
    includegraphics [IGWidth $ Cm 12] "../src/DM/KarnaughMapPOS.pdf" <> lnbk <> parbreak
    mt "C_{min} = \\set{\\text{00X0X, 0XX00, 000X0, 1X1XX, 1XX11, 11XX1, 11X1X}}" <> lnbk <> parbreak
    mt "S^a = 3 + 3 + 4 + 2 + 3 + 3 + 3 = 21, \\quad S^b = 21 + 7 = 28" <> lnbk <> parbreak
    "МКНФ: " <> (math . rendertex $ And [ Or [X 1, X 2, X 4]
                                        , Or [X 1, X 4, X 5]
                                        , Or [X 1, X 2, X 3, X 5]
                                        , Or [Not (X 1), Not (X 3)]
                                        , Or [Not (X 1), Not (X 4), Not (X 5)]
                                        , Or [Not (X 1), Not (X 2), Not (X 5)]
                                        , Or [Not (X 1), Not (X 2), Not (X 4)] ])
