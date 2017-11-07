module CS.Lab4 where

import ReportBase
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Graphicx

writeReport :: IO ()
writeReport = renderFile "./renders/CS-Lab4.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  defineSqcases
  document $ do
    baseTitlePage ("ЛАБОРАТОРНАЯ РАБОТА №4", "Основы вычислительной техники", Just "Вариант 1110", "2017 г.")
    sectionstar "Цель работы"
    "Изучение способов связи между программными модулями, команды обращения к подпрограмме и исследование порядка функционирования БЭВМ при выполнении комплекса взаимосвязанных программ."
    sectionstar "Порядок выполнения работы"
    "Восстановить текст заданного варианта программы и подпрограммы (программного комплекса), определить предназначение и составить его описание, определить область представления и область допустимых значений исходных для подпрограммы, выполнить трассировку программного комплекса."
    sectionstar "Содержание памяти"
    includegraphics [IGWidth (Cm 9)] "../src/CS/Lab4Code.png"
    sectionstar "Исходный текст программы"
    borderedtable [(LeftColumn, 3), (ParColumnTop "10cm", 1)] $ do
      hline
      trow [textbf "Адрес", textbf "Код", textbf "Мнемоника", textbf "Комментарии"]
      tfreerow ["1C9", "F200 " <> textbf "+", "CLA", multirow 2 (Just $ Cm 10) (textbf "Начало программы." <> " Обнулить значение R")] >> cline 1 3
      trow ["1CA", "31E0", "MOV R", ""]
      tfreerow ["1CB", "41DD", "ADD Z", multirow 5 (Just $ Cm 10) "Присвоить R результат работы подпрограммы с исходным значением Z"] >> cline 1 3
      tfreerow ["1CC", "2680", "JSR 680", ""] >> cline 1 3
      tfreerow ["1CD", "F800", "INC", ""] >> cline 1 3
      tfreerow ["1CE", "41E0", "ADD R", ""] >> cline 1 3
      trow ["1CF", "31E0", "MOV R", ""]
      tfreerow ["1D0", "F200", "CLA", multirow 6 (Just $ Cm 10) "Прибавить к R результат работы подпрограммы с исходным значением X + 1"] >> cline 1 3
      tfreerow ["1D1", "41DF", "ADD X", ""] >> cline 1 3
      tfreerow ["1D2", "F800", "INC", ""] >> cline 1 3
      tfreerow ["1D3", "2680", "JSR 680", ""] >> cline 1 3
      tfreerow ["1D4", "41E0", "ADD R", ""] >> cline 1 3
      trow ["1D5", "31E0", "MOV R", ""]
      tfreerow ["1D6", "F200", "CLA", multirow 6 (Just $ Cm 10) "Прибавить к R результат работы подпрограммы с исходным значением Y, увеличенный на 1"] >> cline 1 3
      tfreerow ["1D7", "41DE", "ADD Y", ""] >> cline 1 3
      tfreerow ["1D8", "2680", "JSR 680", ""] >> cline 1 3
      tfreerow ["1D9", "F800", "INC", ""] >> cline 1 3
      tfreerow ["1DA", "41E0", "ADD R", ""] >> cline 1 3
      trow ["1DB", "31E0", "MOV R", ""]
      trow ["1DC", "F000", "HLT", textbf "Конец программы." <> " Остановить ЭВМ"]
      trow ["1DD", "ZZZZ", "Z", "Параметр Z"]
      trow ["1DE", "YYYY", "Y", "Параметр Y"]
      trow ["1DF", "XXXX", "X", "Параметр X"]
      trow ["1E0", "0000", "R", "Результат вычислений"]
    sectionstar "Исходный текст подпрограммы"
    borderedtable [(LeftColumn, 3), (ParColumnTop "10cm", 1)] $ do
      hline
      trow [textbf "Адрес", textbf "Код", textbf "Мнемоника", textbf "Комментарии"]
      trow ["680", "0000", "RET", "Адрес возврата из подпрограммы"]
      tfreerow ["681", "A686", "BMI 686", multirow 3 (Just $ Cm 10) "Перейти к первой ветви, если значение параметра отрицательно или превышает значение константы K"] >> cline 1 3
      tfreerow ["682", "668F", "SUB K", ""] >> cline 1 3
      trow ["683", "9685", "BPL 685", ""]
      trow ["684", "C68B", "BR 68B", "Перейти ко второй ветви, если значение параметра неотрицательно и не превышает значение константы K"]
      tfreerow ["685", "468F", "ADD K", multirow 6 (Just $ Cm 10) (textit "(Ветвь 1)" <> " Выйти из подпрограммы с результатом, равным сумме исходного параметра, умноженного на три, и константы L")] >> cline 1 3
      tfreerow ["686", "368E", "MOV T", ""] >> cline 1 3
      tfreerow ["687", "468E", "ADD T", ""] >> cline 1 3
      tfreerow ["688", "468E", "ADD T", ""] >> cline 1 3
      tfreerow ["689", "4690", "ADD L", ""] >> cline 1 3
      trow ["68A", "CE80", "BR (RET)", ""]
      tfreerow ["68B", "F200", "CLA", multirow 3 (Just $ Cm 10) (textit "(Ветвь 2)" <> " Выйти из подпрограммы с результатом, равным константе K")] >> cline 1 3
      tfreerow ["68C", "468F", "ADD K", ""] >> cline 1 3
      trow ["68D", "CE80", "BR (RET)", ""]
      trow ["68E", "0000", "T", "Временное значение операнда умножения, используемое в первой ветви"]
      trow ["68F", "0B7F", "K", raw "Константа K = $2943_{10}$"]
      trow ["690", "0034", "L", raw "Константа L = $52_{10}$"]
    sectionstar "Описание программы"
    raw "Программа вычисляет значение $R$, которое определеяется как $f(Z) + f(X + 1) + f(Y) + 1$, где $R$, $Z$, $X$, $Y$ -- знаковые числа, представленные 16 разрядами. Начнем с рассмотрения функции $f$:"
    mathDisplay . ("f(x) = " <>) $ cases $ do
      raw "3x + L, & x < 0" >> lnbk
      raw "K, & 0 \\leqslant x \\leqslant K" >> lnbk
      raw "3x + L, & x > K"
    raw "(где $L$ = 2943, $K$ = 52)"
    --raw "\\input{../src/CS/Lab4Graph.latex}" >> lnbk
    vspace (Mm 4) >> parbreak
    raw "Параметр $x$ -- знаковое число, представленное 16 разрядами -- может принимать следующие значения:"
    flalignstar $ do
      environment "sqcases" $ do
        raw "\\ -2^{15} \\leqslant 3x + L \\leqslant 2^{15} - 1" >> lnbk
        raw "\\ 0 < x \\leqslant K"
    vspace (Mm (-6))
    flalignstar $ do
      raw "\\frac{- 2^{15} - L}{3} &\\leqslant x \\leqslant \\frac{2^{15} - 1 - L}{3}" >> lnbk
      raw "-11903 &\\leqslant x \\leqslant 9941"
    parbreak
    raw "Полученное выражение является первым ограничением, накладываемым на область допустимых значений параметров $Z$, $X$ и $Y$, используемых в основной программе. Для предотвращения переполнения $R$ на формулу также распространяется следующее ограничение:"
    flalignstar $ do
      raw " -2^{15} &\\leqslant f(Z) + f(X + 1) + f(Y) + 1 \\leqslant 2^{15} - 1" >> lnbk
      raw "-2^{15} - 1 &\\leqslant f(Z) + f(X + 1) + f(Y) \\leqslant 2^{15} - 2"
    "Законченная система ОДЗ принимает вид:"
    mathDisplay $ cases $ do
      raw "-11903 \\leqslant Z \\leqslant 9941" >> lnbk
      raw "-11903 \\leqslant Y \\leqslant 9941" >> lnbk
      raw "-11902 \\leqslant X \\leqslant 9940" >> lnbk
      raw "-2^{15} - 1 \\leqslant f(Z) + f(X + 1) + f(Y) \\leqslant 2^{15} - 2"
    "Можно выделить такой диапазон значений отдельной переменной, в котором вычислительной ошибки не возникнет независимо от значений остальных параметров:"
    flalignstar $ do
      raw "\\frac{-2^{15} - L}{9} &\\leqslant t \\leqslant \\frac{2^{15} - 1 - L}{9}" >> lnbk
      raw "-3967 &\\leqslant t \\leqslant 3313"
    raw "(где $t$ -- $Z$, $Y$, $X + 1$)"

-- 1C9 a F200 w 31E0 w 41DD w 2680 w F800 w 41E0 w
-- 31E0 w F200 w 41DF w F800 w 2680 w 41E0 w 31E0 w
-- F200 w 41DE w 2680 w F800 w 41E0 w 31E0 w F000 w
-- 0001 w 0000 w FFFF w 0000 w
-- 
-- 680 a 0000 w A686 w 668F w 9685 w C68B w 468F w
-- 368E w 468E w 468E w 4690 w CE80 w F200 w 468F w
-- CE80 w 0000 w 0B7F w 0034 w


