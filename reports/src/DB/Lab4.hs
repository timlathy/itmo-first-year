module DB.Lab4 where

import ReportBase
import Text.LaTeX.Base.Syntax (TeXArg (..))
import Text.LaTeX.Packages.Trees.Qtree

writeReport :: IO ()
writeReport = do
  queries <- readFile "../db/lab4.sql"
  plans <- readFile "../db/lab4-analysis.sql"
  plannerAlgo <- readFile "../db/lab4-query-planner-algo.py"
  renderFile "./renders/DB-Lab4.tex" (execLaTeXM (reportTeX (queries, plans, plannerAlgo)))

reportTeX :: (String, String, String) -> LaTeXM ()
reportTeX (queries, plans, plannerAlgo) = do
  baseHeader
  usepackage [] "fancyvrb"
  usepackage [] "textcomp"
  usepackage [] qtree
  usepackage [] "amsmath"
  usepackage [] "multicol"
  raw "\\DeclareMathOperator*{\\join}{\\bowtie}"
  document $ do
    baseTitlePage ("ЛАБОРАТОРНАЯ РАБОТА №4", "Базы данных", Just "Вариант 2899", "2018 г.")
    sectionstar "Задание"
    enumerate $ do
      item Nothing <> "Сделать запрос для получения атрибутов из указанных таблиц, применив фильтры по указанным условиям:" <> parbreak
      "Таблицы: Н_ЛЮДИ, Н_СЕССИЯ" <> lnbk
      "Вывести атрибуты: Н_ЛЮДИ.ФАМИЛИЯ, Н_СЕССИЯ.ДАТА" <> lnbk
      "Фильтры (AND):" <> lnbk
      "a) Н_ЛЮДИ.ФАМИЛИЯ > Петров" <> lnbk
      "b) Н_СЕССИЯ.ЧЛВК_ИД < 106059" <> lnbk
      "Вид соединения: INNER JOIN."
      item Nothing <> "Сделать запрос для получения атрибутов из указанных таблиц, применив фильтры по указанным условиям" <> parbreak
      environment "flushleft" $ do
        "Таблицы: Н_ЛЮДИ, Н_ОБУЧЕНИЯ, Н_УЧЕНИКИ" <> lnbk
        "Вывести атрибуты: Н_ЛЮДИ.ОТЧЕСТВО, Н_ОБУЧЕНИЯ.ЧЛВК_ИД, Н_УЧЕНИКИ.НАЧАЛО" <> lnbk
        "Фильтры (AND):" <> lnbk
        "a) Н_ЛЮДИ.ИД < 142095" <> lnbk
        "b) Н_ОБУЧЕНИЯ.НЗК > 001000" <> lnbk
        "c) Н_УЧЕНИКИ.ГРУППА < 1101" <> lnbk
        "Вид соединения: INNER JOIN."
    sectionstar "Код запросов"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\small"] $ raw . fromString $ "\n" ++ queries ++ "\n"
    raw "\n"
    newpage
    sectionstar "Вывод EXPLAIN ANALYZE"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\scriptsize"] $ raw . fromString $ "\n" ++ plans ++ "\n"
    raw "\n"
    sectionstar "Планы первого запроса"
    vspace (Mm 4)
    let releq = \operator operands -> raw "{\\Large " <> mt operator <> raw "} {\\scriptsize " <> operands <> raw "}"
    let relation = \r -> raw "{\\small " <> r <> raw "}"
    center $ do
      tree id $ Node (Just $ releq "\\pi" "Н_ЛЮДИ.ФАМИЛИЯ, Н_СЕССИЯ.ДАТА")
        [ Node (Just $ releq "\\join" "Н_ЛЮДИ.ИД = Н_СЕССИЯ.ЧЛВК_ИД")
          [ Node (Just $ releq "\\sigma" "ФАМИЛИЯ < 'Петров'")
            [ Leaf $ relation "Н_ЛЮДИ" ]
          , Node (Just $ releq "\\sigma" "ЧЛВК_ИД < 106059")
            [ Leaf $ relation "Н_СЕССИЯ" ] 
          ]
        ]
      tree id $ Node (Just $ releq "\\pi" "Н_ЛЮДИ.ФАМИЛИЯ, Н_СЕССИЯ.ДАТА")
        [ Node (Just $ releq "\\sigma" "ФАМИЛИЯ < 'Петров'")
          [ Node (Just $ releq "\\sigma" "ЧЛВК_ИД < 106059")
            [ Node (Just $ releq "\\join" "Н_ЛЮДИ.ИД = Н_СЕССИЯ.ЧЛВК_ИД")
              [ Leaf $ relation "Н_ЛЮДИ"
              , Leaf $ relation "Н_СЕССИЯ"
              ]
            ]
          ]
        ]
    parbreak <> "Первый из представленных планов является наиболее оптимальным, так как сокращает число строк, участвующих в операции join, и позволяет использовать индексы для селекции."
    newpage
    sectionstar "Планы второго запроса"
    "Как и для первого запроса, селекцию выгоднее произвести до выполнения операции join."
    center $ tree id $ Node (Just $ releq "\\pi" "Н_ЛЮДИ.ОТЧЕСТВО, Н_ОБУЧЕНИЯ.ЧЛВК_ИД, Н_УЧЕНИКИ.НАЧАЛО")
      [ Node (Just $ releq "\\join" "Н_ЛЮДИ.ИД = Н_УЧЕНИКИ.ЧЛВК_ИД")
        [ Node (Just $ releq "\\join" "Н_ЛЮДИ.ИД = Н_ОБУЧЕНИЯ.ЧЛВК_ИД")
          [ Node (Just $ releq "\\sigma" "ИД < 142095")
            [ Leaf $ relation "Н_ЛЮДИ" ]
          , Node (Just $ releq "\\sigma" "НЗК < '001000'")
            [ Leaf $ relation "Н_ОБУЧЕНИЯ" ] 
          ]
        , Node (Just $ releq "\\sigma" "ГРУППА < '1101'")
          [ Leaf $ relation "Н_УЧЕНИКИ" ]
        ]
      ]
    vspace (Mm 8) <> parbreak
    "Так как в запросе присутствует более двух операций join, его эффективность зависит от порядка их исполнения. Количество выбранных из таблицы Н_УЧЕНИКИ строк в несколько раз меньше, чем из таблицы Н_ОБУЧЕНИЯ, что позволяет нам сделать вывод, что следующий план будет более производительным:"
    center $ tree id $ Node (Just $ releq "\\pi" "Н_ЛЮДИ.ОТЧЕСТВО, Н_ОБУЧЕНИЯ.ЧЛВК_ИД, Н_УЧЕНИКИ.НАЧАЛО")
      [ Node (Just $ releq "\\join" "Н_ЛЮДИ.ИД = Н_ОБУЧЕНИЯ.ЧЛВК_ИД")
        [ Node (Just $ releq "\\join" "Н_ЛЮДИ.ИД = Н_УЧЕНИКИ.ЧЛВК_ИД")
          [ Node (Just $ releq "\\sigma" "ИД < 142095")
            [ Leaf $ relation "Н_ЛЮДИ" ]
          , Node (Just $ releq "\\sigma" "ГРУППА < '1101'")
            [ Leaf $ relation "Н_УЧЕНИКИ" ]
          ]
          , Node (Just $ releq "\\sigma" "НЗК < '001000'")
            [ Leaf $ relation "Н_ОБУЧЕНИЯ" ] 
        ]
      ]
    sectionstar "Анализ первого запроса"
    "Из вывода EXPLAIN ANALYZE следует, что добавление индексов производительность не улучшит." <> parbreak
    "Начнем рассмотрение запроса с условия ЧЛВК_ИД < 106059: ему удовлетворяет большинство строк в таблице, что делает Sequential Scan более эффективной стратегией выборки, чем использование индекса." <> parbreak
    "Планировщик определяет это, используя статистику по таблице. Приблизительный алгоритм оценки выглядит следующим образом:"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\small"] $ raw . fromString $ "\n" ++ plannerAlgo ++ "\n"
    raw "\n"
