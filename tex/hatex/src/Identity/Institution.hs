module Identity.Institution where

import Data.String (IsString)

name :: IsString s => s
name = "ФГАОУ ВО «Санкт-Петербургский\
  \ национальный исследовательский университет\
  \ информационных технологий, механики и оптики»"

department :: IsString s => s
department = "Кафедра вычислительной техники"

location :: IsString s => s
location = "Санкт-Петербург"
