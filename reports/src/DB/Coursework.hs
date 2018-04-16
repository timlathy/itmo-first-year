module DB.Coursework where

import ReportBase
import qualified Identity.Student as Student

import Text.LaTeX.Base.Syntax (TeXArg (..))
import Text.LaTeX.Packages.Graphicx (IGOption (..), includegraphics)

writeReport :: IO ()
writeReport = do
  schema <- readFile "../db/coursework-schema.sql"
  triggers <- readFile "../db/coursework-triggers.sql"
  renderFile "./renders/DB-Coursework.tex" (execLaTeXM $ reportTeX (schema, triggers))

reportTeX :: (String, String) -> LaTeXM ()
reportTeX (schemaSql, triggersSql) = do
  baseHeader
  usepackage [] "fancyvrb"
  usepackage [] "textcomp"
  usepackage [] "multicol"
  document $ do
    multipleAuthorsTitlePage ("КУРСОВАЯ РАБОТА. Часть III", "Базы данных", Nothing, [Student.dbCourseworkGroupmate, Student.name], "2018 г.")
    includegraphics [IGWidth $ Cm 14.4] "../src/DB/Coursework-DDL.png" <> lnbk
    sectionstar "Реализация даталогической модели"
    raw "\\begin{multicols}{2}"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\scriptsize"] $ raw . fromString $ "\n" ++ schemaSql ++ "\n"
    raw "\n"
    raw "\\end{multicols}"
    sectionstar "Реализация триггер-функций"
    environment2 "Verbatim" [OptArg $ raw "fontsize=\\scriptsize"] $ raw . fromString $ "\n" ++ triggersSql ++ "\n"
    raw "\n"
