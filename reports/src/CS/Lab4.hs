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
  raw "\\usepackage{array}\\newcolumntype{P}[1]{>{\\centering\\arraybackslash}p{#1}}"
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
    raw "(где $L$ = 52, $K$ = 2943)"
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
      raw "-10940 &\\leqslant x \\leqslant 10905"
    parbreak
    raw "Полученное выражение является первым ограничением, накладываемым на область допустимых значений параметров $Z$, $X$ и $Y$, используемых в основной программе. Для предотвращения переполнения $R$ на формулу также распространяется следующее ограничение:"
    flalignstar $ do
      raw " -2^{15} &\\leqslant f(Z) + f(X + 1) + f(Y) + 1 \\leqslant 2^{15} - 1" >> lnbk
      raw "-2^{15} - 1 &\\leqslant f(Z) + f(X + 1) + f(Y) \\leqslant 2^{15} - 2"
    "Законченная система ОДЗ принимает вид:"
    mathDisplay $ cases $ do
      raw "-10940 \\leqslant Z \\leqslant 10905" >> lnbk
      raw "-10940 \\leqslant Y \\leqslant 10905" >> lnbk
      raw "-10941 \\leqslant X \\leqslant 10904" >> lnbk
      raw "-2^{15} - 1 \\leqslant f(Z) + f(X + 1) + f(Y) \\leqslant 2^{15} - 2"
    "Можно выделить такой диапазон значений отдельной переменной, в котором вычислительной ошибки не возникнет независимо от значений остальных параметров:"
    flalignstar $ do
      raw "\\frac{-2^{15} - L}{9} &\\leqslant t \\leqslant \\frac{2^{15} - 1 - L}{9}" >> lnbk
      raw "-3646 &\\leqslant t \\leqslant 3635"
    raw "(где $t$ -- $Z$, $Y$, $X + 1$)"

-- 1C9 a F200 w 31E0 w 41DD w 2680 w F800 w 41E0 w
-- 31E0 w F200 w 41DF w F800 w 2680 w 41E0 w 31E0 w
-- F200 w 41DE w 2680 w F800 w 41E0 w 31E0 w F000 w
-- 0001 w 0000 w FFFF w 0000 w
-- 
-- 680 a 0000 w A686 w 668F w 9685 w C68B w 468F w
-- 368E w 468E w 468E w 4690 w CE80 w F200 w 468F w
-- CE80 w 0000 w 0B7F w 0034 w

    newpage
    sectionstar "Таблица трассировки"
    -- Copy-pasted from earlier labs, not sure how to redo the P macro in borderedtable
    raw "\\begin{adjustbox}{center}"
    raw "\\begin{tabular}{| P{1.6cm} | c | c | c | c | c | c | c | P{2.6cm} | P{3.2cm} |}\\hline \
      \\\multicolumn{2}{|P{4cm}}{\\textbf{Выполняемая \\linebreak команда}}\
      \& \\multicolumn{6}{|P{7.2cm}}{\\textbf{Содержимое регистров процессора после выполнения команды}}\
      \& \\multicolumn{2}{|P{6cm}|}{\\textbf{Ячейка, содержимое которой изменилось после выполнения команды}} \\\\" >> hline 
    "Адрес" & "Код" & "СК" & "РА" & "РК" & "РД" & "А" & "С" & "Адрес" & "Новый код" >> lnbk >> hline
    trow ["1C9", "F200", "1CA", "1C9", "F200", "F200", "0000", "0", "", ""]
    trow ["1CA", "31E0", "1CB", "1E0", "31E0", "0000", "0000", "0", "1E0", "0000"]
    trow ["1CB", "41DD", "1CC", "1DD", "41DD", "0001", "0001", "0", "", ""]
    trow ["1CC", "2680", "681", "680", "2681", "01CD", "0001", "0", "680", "01CD"]
    trow ["681", "A686", "682", "681", "A686", "A686", "0001", "0", "", ""]
    trow ["682", "668F", "683", "68F", "668F", "0B7F", "F482", "0", "", ""]
    trow ["683", "9685", "684", "683", "9685", "9685", "F482", "0", "", ""]
    trow ["684", "C68B", "68B", "684", "C68B", "C68B", "F482", "0", "", ""]
    trow ["68B", "F200", "68C", "68B", "F200", "F200", "0000", "0", "", ""]
    trow ["68C", "468F", "68D", "68F", "468F", "0B7F", "0B7F", "0", "", ""]
    trow ["68D", "CE80", "1CD", "680", "CE80", "01CD", "0B7F", "0", "", ""]
    trow ["1CD", "F800", "1CE", "1CD", "F800", "F800", "0B80", "0", "", ""]
    trow ["1CE", "41E0", "1CF", "1E0", "41E0", "0000", "0B80", "0", "", ""]
    trow ["1CF", "31E0", "1D0", "1E0", "31E0", "0B80", "0B80", "0", "1E0", "0B80"]
    trow ["1D0", "F200", "1D1", "1D0", "F200", "F200", "0000", "0", "", ""]
    trow ["1D1", "41DF", "1D2", "1DF", "41DF", "FFFF", "FFFF", "0", "", ""]
    trow ["1D2", "F800", "1D3", "1D2", "F800", "F800", "0000", "1", "", ""]
    trow ["1D3", "2680", "681", "680", "2681", "01D4", "0000", "1", "680", "01D4"]
    trow ["681", "A686", "682", "681", "A686", "A686", "0000", "1", "", ""]
    trow ["682", "668F", "683", "68F", "668F", "0B7F", "F481", "0", "", ""]
    trow ["683", "9685", "684", "683", "9685", "9685", "F481", "0", "", ""]
    trow ["684", "C68B", "68B", "684", "C68B", "C68B", "F481", "0", "", ""]
    trow ["68B", "F200", "68C", "68B", "F200", "F200", "0000", "0", "", ""]
    trow ["68C", "468F", "68D", "68F", "468F", "0B7F", "0B7F", "0", "", ""]
    trow ["68D", "CE80", "1D4", "680", "CE80", "01D4", "0B7F", "0", "", ""]
    trow ["1D4", "41E0", "1D5", "1E0", "41E0", "0B80", "16FF", "0", "", ""]
    trow ["1D5", "31E0", "1D6", "1E0", "31E0", "16FF", "16FF", "0", "1E0", "16FF"]
    trow ["1D6", "F200", "1D7", "1D6", "F200", "F200", "0000", "0", "", ""]
    trow ["1D7", "41DE", "1D8", "1DE", "41DE", "0000", "0000", "0", "", ""]
    trow ["1D8", "2680", "681", "680", "2681", "01D9", "0000", "0", "680", "01D9"]
    trow ["681", "A686", "682", "681", "A686", "A686", "0000", "0", "", ""]
    trow ["682", "668F", "683", "68F", "668F", "0B7F", "F481", "0", "", ""]
    trow ["683", "9685", "684", "683", "9685", "9685", "F481", "0", "", ""]
    trow ["684", "C68B", "68B", "684", "C68B", "C68B", "F481", "0", "", ""]
    trow ["68B", "F200", "68C", "68B", "F200", "F200", "0000", "0", "", ""]
    trow ["68C", "468F", "68D", "68F", "468F", "0B7F", "0B7F", "0", "", ""]
    trow ["68D", "CE80", "1D9", "680", "CE80", "01D9", "0B7F", "0", "", ""]
    trow ["1D9", "F800", "1DA", "1D9", "F800", "F800", "0B80", "0", "", ""]
    trow ["1DA", "41E0", "1DB", "1E0", "41E0", "16FF", "227F", "0", "", ""]
    trow ["1DB", "31E0", "1DC", "1E0", "31E0", "227F", "227F", "0", "1E0", "227F"]
    trow ["1DC", "F000", "1DD", "1DC", "F000", "F000", "227F", "0", "", ""]
    raw "\\end{tabular}"
    raw "\\end{adjustbox}"
