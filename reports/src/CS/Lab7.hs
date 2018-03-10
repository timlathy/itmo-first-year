module CS.Lab7 where

import Text.LaTeX.Packages.Graphicx

import ReportBase

writeReport :: IO ()
writeReport = renderFile "./renders/CS-Lab7.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  raw "\\input{../src/CS/Lab7Head.latex}"
  document $ do
    baseTitlePage ("ЛАБОРАТОРНАЯ РАБОТА №7", "Основы вычислительной техники", Just "Вариант 24", "2018 г.")
    sectionstar "Цель работы"
    "Практическое освоение принципов микропрограммирования и разработки адресных и безадресных команд."
    sectionstar "Задание"
    "Синтезировать цикл исполнения для следующей команды:" <> lnbk <> parbreak
    "MAND M - побитовое И акумулятора с ячейкой памяти с записью результата в ячейку памяти и без установки C/N/Z. Код операции - 7..." <> lnbk <> parbreak
    raw "Разработать тестовые программы, которые проверяют синтезированную команду. Загрузить в микропрограммную память БЭВМ циклы исполнения синтезированных команд, загрузить в основную память БЭВМ тестовые программы, начиная с адреса $4E6_{16}$. Проверить и отладить разработанные тестовые программы и микропрограммы."
    sectionstar "Цикл исполнения команды"
    "Цикл исполнения MAND M состоит из следующих микрокоманд:" <> lnbk <> parbreak
    borderedtable [(ParColumnTop "1.5cm", 1), (ParColumnTop "2cm", 1), (ParColumnTop "3.4cm", 1), (ParColumnTop "8cm", 1)] $ do
      hline
      trow [textbf "Адрес МП", textbf "Микро-команды", textbf "Действие", textbf "Комментарии"]
      trow ["B0", "1120", raw "A \\& РД \\rightarrow\\  БР", "Логическое & операнда и аккумулятора"]
      trow ["B1", "4002", raw "БР \\rightarrow\\  РД", "Пересылка результата в РД"]
      trow ["B2", "0002", raw "РД \\rightarrow\\  ОП(РА)", "Запись результата по адресу операнда"]
      trow ["B3", "8390", raw "GOTO INTR(90)", "Переход на цикл прерывания"]
    lnbk <> parbreak
    "Цикл исполнения можно сократить на одну команду, увеличив при этом время исполнения, за счет частичного использования микрокода команды MOV:" <> lnbk <> parbreak
    borderedtable [(ParColumnTop "1.5cm", 1), (ParColumnTop "2cm", 1), (ParColumnTop "3.4cm", 1), (ParColumnTop "8cm", 1)] $ do
      hline
      trow [textbf "Адрес МП", textbf "Микро-команды", textbf "Действие", textbf "Комментарии"]
      trow ["B0", "1120", raw "A \\& РД \\rightarrow\\  БР", "Логическое & операнда и аккумулятора"]
      trow ["B1", "4002", raw "БР \\rightarrow\\  РД", "Пересылка результата в РД"]
      trow ["B3", "833A", raw "GOTO 3A", "Переход к микрокоду MOV"]
    sectionstar "Таблица трассировки цикла исполнения 74E0 (MAND 4E0)"
    raw "\\begin{adjustbox}{center}"
    raw "\\begin{tabular}{| P{2.8cm} | c | c | c | c | c | c | c | c | c | c | c |} \\hline \\multirow{2}{*}{\\parbox{2.9cm}{\\centering\\textbf{СчМК до выборки \\\\ МК}}} & \\multicolumn{11}{P{12cm}|}{\\textbf{Содержимое памяти и регистров процессора после выборки и исполнения микрокоманды}} \\\\ \\cline{2-12} & МК & СК & РА & РК & РД & А & C & БР & N & Z & СчМК \\\\ \\hline "
    trow ["B0", "1120", "528", "4E0", "74E0", "7001", "8000", "1", "00000", "1", "0", "B1"]
    trow ["B1", "4002", "528", "4E0", "74E0", "0000", "8000", "1", "00000", "1", "0", "B2"]
    trow ["B2", "0002", "528", "4E0", "74E0", "0000", "8000", "1", "00000", "1", "0", "B3"]
    trow ["B3", "8390", "528", "4E0", "74E0", "0000", "8000", "1", "00005", "1", "0", "90"]
    raw "\\end{tabular}"
    raw "\\end{adjustbox}"
    sectionstar "Листинг тестовой программы"
    raw "\\input{../src/CS/Lab7Listing.latex}"
    newpage
    sectionstar "Конвейерное исполнение безадресной команды ROL [F600]"
    raw "\\hspace*{-1cm}"
    borderedtable [(CenterColumn, 1), (ParColumnTop "5.2cm", 1), (ParColumnTop "5.2cm", 1), (ParColumnTop "5.2cm", 1)] $ do
      let pwrap t = raw "\\parbox{5.2cm}{" <> t <> raw "}"
      let mov = \r1 r2 -> r1 <> raw "\\\\\\rightarrow\\ " <> r2
      let movl = \r1 r2 -> r1 <> raw "\\rightarrow\\ "<> r2
      hline
      trow [textbf "Такт", textbf "Выборка команды", textbf "Выборка адреса", textbf "Исполнение"]
      trow ["1", "СК" `movl` "РА"
               , pwrap $ "РК ВК/ВА" `mov` "РК ВА/ИСП"
               , pwrap $ "8-11 биты РК ВА/ИСП" `mov` "СчМК"]
      trow ["2", "Память(РА)" `movl` "РД"
               , pwrap $ "РК ВК/ВА" `mov` "адресная? в ВА/ИСП" `mov` "косвенная? в ВА/ИСП"
               , texttt "06: GOTO ROL(82)"]
      trow ["3", "РД" `movl` "РК ВК/ВА", "", texttt "82: RAL(A)" `movl` texttt "БР"]
      trow ["4", "СК + 1", "", texttt "83: БР" `movl` texttt "A, C, N, Z"]
      trow ["5", "", "" , texttt "84: GOTO ПРЕ(90)"]
      trow ["6", "", "" , texttt "90: IF BIT(5,PC) = 0 THEN СЛЕД(FE)"]
    raw "\\hspace*{-1cm}"
    sectionstar "Конвейерное исполнение команды с косвенной адресацией ADD (007) [4807]"
    raw "\\hspace*{-1cm}"
    borderedtable [(CenterColumn, 1), (ParColumnMid "5.2cm", 1), (ParColumnMid "5.2cm", 1), (ParColumnMid "5.2cm", 1)] $ do
      let pwrap t = raw "\\parbox{5.2cm}{" <> t <> raw "}"
      let mov = \r1 r2 -> r1 <> raw "\\\\\\rightarrow\\ " <> r2
      let movl = \r1 r2 -> r1 <> raw "\\ \\rightarrow\\ "<> r2
      hline
      trow [textbf "Такт", textbf "Выборка команды", textbf "Выборка адреса", textbf "Исполнение"]
      trow ["1", "СК" `movl` "РА"
               , pwrap $ "РК ВК/ВА" `mov` "РК ВА/ИСП"
               , pwrap $ "8-11 биты РК ВА/ИСП" `mov` "СчМК"]
      trow ["2", "Память(РА)" `movl` "РД"
               , pwrap $ "РК ВК/ВА" `mov` "адресная? в ВА/ИСП" `mov` "косвенная? в ВА/ИСП"
               , texttt "11: IF BIT(3,РО) = 0 THEN АДР(1D)"]
      trow ["3", "РД" `movl` "РК ВК/ВА"
               , "РК ВК/ВА" `movl` "РА"
               , texttt "1D: IF BIT(15,PK) = 1 THEN ПРХ(2D)"]
      trow ["4", "СК + 1"
               , "Память(РА)" `movl` "РД"
               , texttt "1E: РО" `movl` texttt "БР"]
      trow ["5", ""
               , "РД" `movl` "РО ВК/ВА"
               , texttt "1F: БР" `movl` texttt "РА"]
      trow ["6", "", "", texttt "20: IF BIT(14,PK) = 1 THEN АРФ(27)"]
      trow ["6", "", "", texttt "27: Память(РА)" `movl` texttt "РД"]
      trow ["7", "", "", texttt "28: IF BIT(13,PK) = 0 THEN СУМ(2B)"]
      trow ["8", "", "", texttt "2B: IF BIT(12,PK) = 0 THEN ADD(3C)"]
      trow ["9", "", "", texttt "3C: A + РД" `movl` texttt "А"]
      trow ["10", "", "", texttt "3D: БР" `movl` texttt "A, C, N, Z"]
      trow ["11", "", "", texttt "3E: GOTO ПРЕ(90)"]
      trow ["12", "", "" , texttt "90: IF BIT(5,PC) = 0 THEN СЛЕД(FE)"]
    raw "\\hspace*{-1cm}"
    sectionstar "Конвейерное исполнение команды перехода BR 307 [C307]"
    raw "\\hspace*{-1cm}"
    borderedtable [(CenterColumn, 1), (ParColumnMid "5.2cm", 1), (ParColumnMid "5.2cm", 1), (ParColumnMid "5.2cm", 1)] $ do
      let pwrap t = raw "\\parbox{5.2cm}{" <> t <> raw "}"
      let mov = \r1 r2 -> r1 <> raw "\\\\\\rightarrow\\ " <> r2
      let movl = \r1 r2 -> r1 <> raw "\\ \\rightarrow\\ "<> r2
      hline
      trow [textbf "Такт", textbf "Выборка команды", textbf "Выборка адреса", textbf "Исполнение"]
      trow ["1", "СК" `movl` "РА"
               , pwrap $ "РК ВК/ВА" `mov` "РК ВА/ИСП"
               , pwrap $ "8-11 биты РК ВА/ИСП" `mov` "СчМК"]
      trow ["2", "Память(РА)" `movl` "РД"
               , pwrap $ "РК ВК/ВА" `mov` "адресная? в ВА/ИСП" `mov` "косвенная? в ВА/ИСП"
               , texttt "11: IF BIT(3,РО) = 0 THEN АДР(1D)"]
      trow ["3", "РД" `movl` "РК ВК/ВА", ""
               , texttt "1D: IF BIT(15,PK) = 1 THEN ПРХ(2D)"]
      trow ["4", "СК + 1", ""
               , texttt "2D: IF BIT(14,PK) = 0 THEN УПХ(30)"]
      trow ["5", "", ""
               , texttt "2E: IF BIT(12,PK) = 0 THEN BR(47)"]
      trow ["6", "", "", texttt "47: РД" `movl` texttt "БР"]
      trow ["7", "", "", texttt "48: БР" `movl` texttt "СК"]
      trow ["8", "", "", texttt "3E: GOTO ПРЕ_NOP(AE)"]
      trow ["9", "", "" , texttt "AE: IF BIT(5,PC) = 0 THEN СЛЕД_NOP(FF)"]
    raw "\\hspace*{-1cm}"
    sectionstar "Ограничения, видимые пользователю (программисту)"
    "Отстутствует проверка наличия " <> textit "data hazard (read after write)" <> ", следовательно:"
    enumerate $ do
      item Nothing <> "Команды с косвенной адресацией, следующие за MOV, ISZ, JSR, изменяющими содержание ячейки с адресом операнда, считают неверный адрес. Для разрешения возможных конфликтов после изменения памяти вставляется команда NOP."
      item Nothing <> "Команды, изменяемые во время выполнения, должны быть отделены хотя бы двумя несвязанными командами от изменяющих их."  
    newpage
    includegraphics [IGWidth (Cm 18)] "../src/CS/PipelineIdeas.pdf"
