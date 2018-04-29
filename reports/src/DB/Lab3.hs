module DB.Lab3 where

import ReportBase
import Text.LaTeX.Base.Syntax (TeXArg (..))

writeReport :: IO ()
writeReport = do
  queries <- readFile "../db/lab3.sql"
  plans <- readFile "../db/lab3-analysis.sql"
  renderFile "./renders/DB-Lab3.tex" (execLaTeXM (reportTeX (queries, plans)))

reportTeX :: (String, String) -> LaTeXM ()
reportTeX (queries, plans) = do
  baseHeader
  usepackage [] "fancyvrb"
  usepackage [] "textcomp"
  document $ do
    baseTitlePage ("ЛАБОРАТОРНАЯ РАБОТА №3", "Базы данных", Just "Вариант 2894", "2018 г.")
    sectionstar "Задание"
    enumerate $ do
      item Nothing <> "Сделать запрос для получения атрибутов из указанных таблиц, применив фильтры по указанным условиям:" <> parbreak
      "Таблицы: Н_ЛЮДИ, Н_СЕССИЯ." <> lnbk
      "Вывести атрибуты: Н_ЛЮДИ.ОТЧЕСТВО, Н_СЕССИЯ.ЧЛВК_ИД." <> lnbk
      "Фильтры (AND):" <> lnbk
      "a) Н_ЛЮДИ.ОТЧЕСТВО < Георгиевич." <> lnbk
      "b) Н_СЕССИЯ.ИД > 14." <> lnbk
      "c) Н_СЕССИЯ.ИД < 14." <> lnbk
      "Вид соединения: LEFT JOIN."
      item Nothing <> "Сделать запрос для получения атрибутов из указанных таблиц, применив фильтры по указанным условиям:" <> parbreak
      "Таблицы: Н_ЛЮДИ, Н_ВЕДОМОСТИ, Н_СЕССИЯ." <> lnbk
      "Вывести атрибуты: Н_ЛЮДИ.ИМЯ, Н_ВЕДОМОСТИ.ЧЛВК_ИД, Н_СЕССИЯ.ДАТА." <> lnbk
      "Фильтры (AND):" <> lnbk
      "a) Н_ЛЮДИ.ИД = 100012." <> lnbk
      "b) Н_ВЕДОМОСТИ.ИД > 1426978." <> lnbk
      "Вид соединения: INNER JOIN."
      item Nothing <> "Вывести число имен без учета повторений." <> parbreak
      "При составлении запроса нельзя использовать DISTINCT."
      item Nothing <> "В таблице Н_ГРУППЫ_ПЛАНОВ найти номера планов, по которым обучается (обучалось) более 2 групп на кафедре вычислительной техники." <> parbreak
      "Для реализации использовать соединение таблиц."
      item Nothing <> "Выведите таблицу со средним возрастом студентов во всех группах (Группа, Средний возраст), где средний возраст равен максимальному возрасту в группе 1101."
      item Nothing <> "Получить список студентов, отчисленных до первого сентября 2012 года с очной или заочной формы обучения (специальность: 230101). В результат включить:" <> parbreak
      "номер группы;" <> lnbk
      "номер, фамилию, имя и отчество студента;" <> lnbk
      "номер пункта приказа;" <> lnbk
      "Для реализации использовать подзапрос с IN."
      item Nothing <> "Сформировать запрос для получения числа в СПбГУ ИТМО отличников."
    sectionstar "Код запросов"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\small"] $ raw . fromString $ "\n" ++ queries ++ "\n"
    raw "\n"
    sectionstar "Планы выполнения первого и второго запроса"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\scriptsize"] $ raw . fromString $ "\n" ++ plans ++ "\n"
    raw "\n"
