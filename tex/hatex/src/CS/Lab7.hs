module CS.Lab7 where

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
    sectionstar "Листинг тестовой программы"
    raw "\\input{../src/CS/Lab7Listing.latex}"
