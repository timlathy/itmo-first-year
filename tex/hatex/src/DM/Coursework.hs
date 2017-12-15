module DM.Coursework where

import Data.List (intersperse)
import Text.LaTeX.Packages.AMSMath (math, mathDisplay, cases)
import Text.LaTeX.Packages.Graphicx (IGOption (..), includegraphics)

import ReportBase
import DM.Logic
import DM.CourseworkTruthTable
import DM.QuineMcCluskey
import qualified DM.CourseworkSecondTable as SecondTable

writeReport :: IO ()
writeReport = renderFile "./renders/DM-Coursework.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  defineWidebar
  usepackage [] "caption"
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
    sectionstar "Синтез комбинационной схемы в универсальном базисе"
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
    sectionstar "Синтез комбинационной схемы в сокращенном булевом базисе"
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
    lnbk <> vspace (Mm 6) <> parbreak
    includegraphics [IGWidth $ Cm 15] "../src/DM/NOT_ORCircuit.pdf" <> lnbk <> parbreak
    newpage
    baseTitlePage ("Курсовая работа", "Дискретная математика", Just "Вариант 61", "2017 г.")
    sectionstar "Синтез сумматора"
    "Комбинационная схема должна выполнять операцию сложения двух трехразрядных знаковых двоичных чисел, представленных в прямом коде, с фиксацией переполнения:"
    mathDisplay $ do
      "C = A + B" <> raw "\\text{, где } A = (a_{sign}, a_1, a_2), B = (b_{sign}, b_1, b_2), C = (c_{overflow}, c_{sign}, c_{1}, c_{2})"    
    sectionstar "Составление таблицы истинности"
    SecondTable.truthTableTeX
    sectionstar "Минимизация булевых функций системы. Поиск МДНФ"
    let cubeList = mconcat . (intersperse ",") . (fromString <$>)
    -- overflow
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughCOverflow.pdf" <> lnbk <> parbreak
    let csOverflow = ["0110X1", "0X1011", "01X01X", "1111X1", "1X1111", "11X11X"]
    mt ("C_{overflow} = \\{" <> cubeList csOverflow <> "\\}") <> lnbk <> parbreak
    -- sign
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughCSign.pdf" <> lnbk <> parbreak
    let csSign = ["X001X1", "XX0111", "X0X11X", "1X1X00", "111XX0", "11XX0X", "1XX1X1", "11X11X"]
    mt ("C_{sign} = \\{" <> cubeList csSign <> "\\}") <> lnbk <> parbreak
    -- c1
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughC1.pdf" <> lnbk
    let cs1 = ["X1XX00", "X00X1X", "001001", "01000X", "011011", "00X010", "11100X", "10X011", "01110X", "00X111", "101101", "11010X", "111111", "10X110"]
    flalignstar $ do
      raw ("C_1 = \\{&" <> cubeList (take 6 cs1) <> ",") <> lnbk
      raw ("&" <> cubeList (drop 6 cs1) <> "\\}") <> lnbk
    -- c2
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughC2.pdf" <> lnbk
    let cs2 = ["XX1XX0", "XX0XX1"]
    flalignstar $ raw ("C_2 = \\{&" <> cubeList cs2 <> "\\}") <> lnbk
    -- cases
    mathDisplay $ cases $ do
      raw "C_{overflow} =" <> SecondTable.renderCubesInChunksOf 3 csOverflow <> lnbk
      raw "C_{sign} =" <> SecondTable.renderCubesInChunksOf 3 csSign <> lnbk
      raw "C_1 =" <> SecondTable.renderCubesInChunksOf 3 cs1 <> lnbk
      raw "C_2 =" <> SecondTable.renderCubesInChunksOf 3 cs2
    flalignstar $ do
      raw "&S_Q^{C_{overflow}} =\\ " <> SecondTable.sQuineEq csOverflow <> lnbk
      raw "&S_Q^{C_{sign}} =\\ "  <> SecondTable.sQuineEq csSign <> lnbk
      raw "&S_Q^{C_1} =\\ "  <> SecondTable.sQuineEq cs1 <> lnbk
      raw "&S_Q^{C_2} =\\ "  <> SecondTable.sQuineEq cs2 <> lnbk
      raw "&S_Q^{\\Sigma} =\\ " <> fromIntegral (SecondTable.sQuineSum [csOverflow, csSign, cs1, cs2]) <> lnbk
    ----
    newpage
    sectionstar "Минимизация булевых функций системы. Поиск МКНФ"
    -- overflow
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughPoSCOverflow.pdf" <> lnbk <> parbreak
    let posOverflow = ["XXXX00", "XX0X0X", "X00XXX", "X0XX0X", "X0XXX0", "0XX1XX", "1XX0XX"]
    mt ("C_{overflow}(\\widebar{f}) = \\{" <> cubeList posOverflow <> "\\}") <> lnbk <> parbreak
    -- sign
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughPoSCSign.pdf"
    let posSign = ["X00X00", "XX001X", "01XXX0", "0X1X0X", "01XX0X", "011XXX", "X0X0X1", "X0X01X", "XXX011"]
    flalignstar $ do
      raw ("C_{sign}(\\widebar{f}) = \\{&" <> cubeList (take 6 posSign) <> ",") <> lnbk
      raw ("&" <> cubeList (drop 6 posSign) <> "\\}") <> lnbk
    -- c1
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughPoSC1.pdf"
    let posC1 = [ "X00X0X", "X0XX00", "011001", "001011", "X10X1X", "X1XX10", "00X10X", "0101X1", "0X1110"
                , "01X11X", "10X00X", "1100X1", "1X1010", "11X01X", "111101", "101111"]
    flalignstar $ do
      raw ("C_{1}(\\widebar{f}) = \\{&" <> cubeList (take 6 posC1) <> ",") <> lnbk
      raw ("&" <> cubeList (take 6 (drop 6 posC1)) <> ",") <> lnbk
      raw ("&" <> cubeList (drop 12 posC1) <> "\\}") <> lnbk
    -- c2
    includegraphics [IGWidth $ Cm 14] "../src/DM/KarnaughPoSC2.pdf" <> lnbk <> parbreak
    let posC2 = ["XX0XX0", "XX1XX1"]
    mt ("C_2(\\widebar{f}) = \\{" <> cubeList posC2 <> "\\}") <> lnbk <> parbreak
-- cases
    mathDisplay $ cases $ do
      raw "C_{overflow} =" <> SecondTable.renderPoSCubesInChunksOf 4 posOverflow <> lnbk
      raw "C_{sign} =" <> SecondTable.renderPoSCubesInChunksOf 4 posSign <> lnbk
      raw "C_1 =" <> SecondTable.renderPoSCubesInChunksOf 3 posC1 <> lnbk
      raw "C_2 =" <> SecondTable.renderPoSCubesInChunksOf 4 posC2
    flalignstar $ do
      raw "&S_Q^{C_{overflow}} =\\ " <> SecondTable.sQuineEq posOverflow <> lnbk
      raw "&S_Q^{C_{sign}} =\\ "  <> SecondTable.sQuineEq posSign <> lnbk
      raw "&S_Q^{C_1} =\\ "  <> SecondTable.sQuineEq posC1 <> lnbk
      raw "&S_Q^{C_2} =\\ "  <> SecondTable.sQuineEq posC2 <> lnbk
      raw "&S_Q^{\\Sigma} =\\ " <> fromIntegral (SecondTable.sQuineSum [posOverflow, posSign, posC1, posC2]) <> lnbk

