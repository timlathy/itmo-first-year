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
  usepackage [] "caption"
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
    newpage ------
    sectionstar "Нахождение МДНФ методом Квайна--Мак-Класки"
    textit "Нахождение простых импликант (максимальных кубов):" <> lnbk <> parbreak
    (cubeTable cubegroups numOfOnes) <> newpage
    textit "Составление импликантной таблицы:" <> lnbk <> parbreak
    (implicantTable minterms cubegroups) <> lnbk <> parbreak
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
    newpage ------
--    sectionstar "Нахождение МКНФ методом Квайна--Мак-Класки"
--    textit "Нахождение простых импликант (максимальных кубов):" <> lnbk <> parbreak
--    (cubeTable cubegroupspos numOfZeroes) <> newpage
--    textit "Составление импликантной таблицы:" <> lnbk <> parbreak
--    (implicantTable maxterms cubegroupspos) <> lnbk <> parbreak
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
    newpage
    sectionstar "Преобразование минимальных форм булевой функции"
    raw "\\quad" <> lnbk
    textbf "Факторное преобразование для МДНФ:"
    flalignstar $ do
      let transform1 = Or [ And [Not (X 1), X 3]
                          , And [Not (X 1), X 2, X 4]
                          , And [Not (X 1), X 2, X 5]
                          , And [Not (X 1), X 4, X 5]
                          , And [X 1, Not (X 2), Not (X 4)]
                          , And [X 1, Not (X 4), Not (X 5)]
                          , And [X 1, Not (X 2), Not (X 3), Not (X 5)] ]
      rendertex transform1 <> raw ("&\\quad\\quad S_Q = " <> fromString (show $ costQ transform1)) <> lnbk
      let transform2 = Or [ And [Not (X 1), Or [X 3, And [X 2, X 4], And [X 2, X 5], And [X 4, X 5]]]
                          , And [X 1, Or [ And [Not (X 2), Not (X 4)]
                                         , And [Not (X 4), Not (X 5)]
                                         , And [Not (X 2), Not (X 3), Not (X 5)]]] ]
      rendertex transform2 <> raw ("&\\quad\\quad S_Q = " <> fromString (show $ costQ transform2)) <> lnbk
      let transform3 = Or [ And [Not (X 1), Or [X 3, And [X 4, Or [X 2, X 5]], And [X 2, X 5]]]
                          , And [X 1, Or [ And [Not (X 4), Or [Not (X 2), Not (X 5)]]
                                         , And [Not (X 2), Not (X 3), Not (X 5)]]] ]
      rendertex transform3 <> raw ("&\\quad\\quad S_Q = " <> fromString (show $ costQ transform3) <> "&")
    textbf "Факторное преобразование для МКНФ:"
    flalignstar $ do
      let transform1 = And [ Or [X 1, X 2, X 4]
                           , Or [X 1, X 4, X 5]
                           , Or [X 1, X 2, X 3, X 5]
                           , Or [Not (X 1), Not (X 3)]
                           , Or [Not (X 1), Not (X 4), Not (X 5)]
                           , Or [Not (X 1), Not (X 2), Not (X 5)]
                           , Or [Not (X 1), Not (X 2), Not (X 4)] ]
      raw "&" <> rendertex transform1 <> lnbk <> raw ("&\\quad S_Q = " <> fromString (show $ costQ transform1)) <> lnbk
      let transform2 = And [ Or [X 1, And [Or [X 2, X 4], Or [X 4, X 5], Or [X 2, X 3, X 5]]]
                           , Or [Not (X 1), And [ Not (X 3)
                                                , Or [Not (X 4), Not (X 5)]
                                                , Or [Not (X 2), Not (X 5)]
                                                , Or [Not (X 2), Not (X 4)] ]]]
      raw "&" <> rendertex transform2 <> lnbk <> raw ("&\\quad S_Q = " <> fromString (show $ costQ transform2)) <> lnbk
      let transform3 = And [ Or [X 1, And [Or [X 4, And [X 2, X 5]], Or [X 2, X 3, X 5]]]
                           , Or [Not (X 1), And [ Not (X 3)
                                                , Or [Not (X 4), And [Not (X 5), Not (X 2)]]
                                                , Or [Not (X 2), Not (X 5)] ]]]
      raw "&" <> rendertex transform3 <> lnbk <> raw ("&\\quad S_Q = " <> fromString (show $ costQ transform3)) <> lnbk
      let transform4 = Or [ And [X 1, Not (X 3)
                                    , Or [Not (X 4), And [Not (X 5), Not (X 2)]]
                                    , Or [Not (X 2), Not (X 5)]]
                          , And [Not (X 1), Or [X 4, And [X 2, X 5]], Or [X 2, X 3, X 5]] ]
      raw "&" <> rendertex transform4 <> lnbk <> raw ("&\\quad S_Q = " <> fromString (show $ costQ transform4) <> "&")
    raw "Введем вспомогательную функцию $\\varphi = " <> rendertex (Or [Not (X 2), Not (X 5)]) <> raw "$,"
    raw "$\\enspace\\widebar{\\varphi} = " <> rendertex (And [X 2, X 5])
    raw "\\enspace(S_Q = " <> fromString (show $ costQ $ Or [Not (X 2), Not (X 5)]) <> raw ")$:"
    flalignstar $ do
      raw "&x_1\\widebar{x_3}(\\widebar{x_4} \\lor \\widebar{x_2}\\widebar{x_5})\\varphi\\lor"
      raw "\\widebar{x_1}(x_4 \\lor \\widebar{\\varphi})(x_2 \\lor x_3 \\lor x_5)" <> lnbk
      let phonyFunForSQ = Or [ And [X 1, Not (X 3), Or [Not (X 4), And [Not (X 5), Not (X 2)], X 10]]
                             , And [Not (X 1), Or [X 4, X 10], Or [X 2, X 3, X 5]] ]
      let costq1 = costQ phonyFunForSQ
      raw "&\\quad S_Q = " <> fromString (show costq1) <> " + 3 = " <> fromString (show $ costq1 + 3) <> raw "&"
    "Полученное в результате декомпозиции выражение обладает наименьшей ценой схемы при условии, что схема строится на элементах булева базиса с парафазными входами."
    newpage
    sectionstar "Синтез комбинационных схем в булевом базисе"
    minipage (Just Top) (".34" <> textwidth) $ do
      vspace (Mm 1)
      "Комбинационная схема с парафазными входами, реализующая последнее выражение, представлена справа." <> lnbk <> lnbk
      raw "Задержка схемы $T=5\\tau$, цена $S_Q=22$." <> lnbk
    hfill
    minipage (Just Top) (".58" <> textwidth) $ do
      vspace (Mm 1)
      includegraphics [IGWidth $ Cm 9] "../src/DM/BooleanLogicCircuitParaPh.pdf" <> lnbk <> parbreak
    parbreak <> vspace (Mm 6)
    minipage (Just Top) (".34" <> textwidth) $ do
      vspace (Mm 1)
      "При посмотрении схемы с однофазными входами было учтено, что выражение, не обладающее минимальной ценой, но имеющее наименьшее число инверсных входных переменных, может оказаться более оптимальным." <> lnbk
      "Каждое из преобразованных выражений содержит инверсии всех пяти переменных, поэтому для построения схемы с однофазными входами использовалось то же выражение, что и для построения с парафазными входами." <> lnbk <> lnbk
      raw "Задержка схемы $T=6\\tau$, цена $S_Q=22 + 5 = 27$." <> lnbk <> parbreak
    hfill
    minipage (Just Top) (".58" <> textwidth) $ do
      vspace (Mm 1)
      includegraphics [IGWidth $ Cm 11] "../src/DM/BooleanLogicCircuitSinglePh.pdf" <> lnbk <> parbreak
    newpage
    sectionstar "Синтез комбинационных схем в универсальных базисах"
    "Приведем функцию к базису И-НЕ, используя законы двойственности:"
    flalignstar $ do
      raw "&x_1\\widebar{x_3}(\\widebar{x_4} \\lor \\widebar{x_2}\\widebar{x_5})\\varphi\\lor"
      raw "\\widebar{x_1}(x_4 \\lor \\widebar{\\varphi})(x_2 \\lor x_3 \\lor x_5) = " <> lnbk
      raw "&(\\widebar{x_1 \\mid \\widebar{x_3}})(x_4 \\mid (\\widebar{x_2} \\mid \\widebar{x_5}))\\varphi"
      raw " \\lor \\widebar{x_1}(\\widebar{x_4} \\mid \\varphi)"
      raw "((\\overline{\\widebar{x_2} \\mid \\widebar{x_3}}) \\mid \\widebar{x_5}) = " <> lnbk
      raw "&(((\\overline{(x_1 \\mid \\widebar{x_3}) \\mid (x_4 \\mid (\\widebar{x_2} \\mid \\widebar{x_5})})"
      raw " \\mid \\varphi) \\mid "
      raw "((\\overline{\\widebar{x_1} \\mid (\\widebar{x_4} \\mid \\varphi)}) \\mid"
      raw "((\\overline{\\widebar{x_2} \\mid \\widebar{x_3}}) \\mid \\widebar{x_5}))"
    parbreak
    raw "Вспомогательная функция $\\varphi$ примет следующий вид:"
    flalignstar $ do
      raw "\\varphi = \\widebar{x_2} \\lor \\widebar{x_5} = \\widebar{x_2x_5} = x_2 \\mid x_5"
    parbreak
    raw "Цена схемы $S_Q = 28 + 3 = 31$, задержка $T = 6\\tau$." <> lnbk <> vspace (Mm 6) <> parbreak
    includegraphics [IGWidth $ Cm 15] "../src/DM/NANDCircuit.pdf" <> lnbk <> parbreak
    newpage
    sectionstar "Синтез комбинационных схем в сокращенных булевых базисах"
    "Функцию выгоднее представить в базисе ИЛИ, НЕ, что связано с отсутствием необходимости выходного инвертора. Преобразуем исходное выражение:"
    flalignstar $ do
      raw "&x_1\\widebar{x_3}(\\widebar{x_4} \\lor \\widebar{x_2}\\widebar{x_5})\\varphi\\lor"
      raw "\\widebar{x_1}(x_4 \\lor \\widebar{\\varphi})(x_2 \\lor x_3 \\lor x_5) = " <> lnbk
      raw "&(\\overline{\\widebar{x_1} \\lor x_3 \\lor (\\overline{\\widebar{x_4} \\lor \\overline{x_2 \\lor x_5}}) \\lor \\varphi})"
      raw " \\lor (\\overline{x_1 \\lor (\\overline{x_4 \\lor \\widebar{\\varphi}}) \\lor (\\overline{x_2 \\lor x_3 \\lor x_5})})" <> lnbk
    parbreak
    raw "Вспомогательное выражение $\\varphi = \\widebar{x_2} \\lor \\widebar{x_5}$ останется без изменения."
    lnbk <> parbreak
    raw "Цена схемы с парафазными входами равна $S_Q = 25 + 3 \\ (\\varphi) + 1 \\ (\\text{инвертор } \\varphi) = 29$."


