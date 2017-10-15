{-# LANGUAGE AllowAmbiguousTypes #-}

module Inf.Lab3 where

import Data.List
import Data.Function (on)

import qualified Inf.Lab3Id as Id
import ReportBase

import Text.LaTeX.Packages.AMSMath

writeReport :: IO ()
writeReport = renderFile "./renders/Inf-Lab3.tex" (execLaTeXM reportTeX)

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  document $ do
    baseTitlePage ("ЛАБОРАТОРНАЯ РАБОТА №3", "Информатика", "2017 г.")
    sectionstar "Цель работы"
    "Овладеть навыками представления данных в различных кодировках."
    sectionstar "Задания"
    enumerate $ do
      item Nothing <> do
        textit "Сформировать сообщение, взяв первые 9 букв от фамилии, от имени и от отчества в верхнем регистре. Если букв оказалось меньше 27, дополнить сообщение чередованием букв А, О, П, Р." >> lnbreak (Mm 1) >> newline
        "Получившееся сообщение: " >> texttt (fromString Id.message)
      item Nothing <> do
        textit "Рассчитать количество информации (энтропию) в полученном сообщении (в битах), используя меру Шеннона: в качестве события принять появление некоторой буквы и рассчитать вероятность её появления, исходя из частоты встречаемости букв в сообщении." >> lnbk
        minipage (Just Top) (".38" <> textwidth) $ do
          vspace (Mm 1)
          tabular Nothing [VerticalLine, ParColumnTop "3cm", VerticalLine, ParColumnTop "2cm", VerticalLine] $ do
            hline >> (textbf "Буква" & textbf "Частота") >> lnbk >> hline
            letterFrequencyRows Id.message
        minipage (Just Top) (".52" <> textwidth) $ do
          vspace (Mm 1)
          "Найдем информационную энтропию полученного сообщения, используя следующую формулу:"
          flalignstar (raw "&" >> "-" >> raw "\\sum_{i=1}^N p_i * \\log_2 p_i" >> raw "&")
          "При " >> math "N = 16" >> ", длине сообщения, равной 27, и частоте встречаемости букв, приведенной в таблице, получаем:"
          flalignstar $ do
            raw "&" >> raw "p_{1..3} = " >> frac 3 27 >> quad >> raw "p_{1..3} * log_2 p_{1..3} \\approx -0,35" >> lnbk
            raw "&" >> raw "p_{4..9} = " >> frac 2 27 >> quad >> raw "p_{4..9} * log_2 p_{4..9} \\approx -0,28" >> lnbk
            raw "&" >> raw "p_{10..16} = " >> frac 1 27 >> quad >> raw "p_{10..16} * log_2 p_{10..16} \\approx -0,18" >> lnbreak (Mm 4)
            raw "&" >> raw "i \\approx 3,99" >> raw "&"
      item Nothing <> do
        textit "Записать полученную последовательность в одну строку, посчитать её размер, учитывая, что для хранения используется кодировка UTF-16, т.е. на 1 символ необходимо 2 байта." >> lnbreak (Mm 1) >> newline
        texttt (fromString Id.message) >> ", длина строки = 27, размер = " >> math "27 * 2 = 54" >> " байта."
      item Nothing <> do
        textit " Используя двоичный код, составить код постоянной длины, с помощью которого можно закодировать исходное сообщение при использовании минимального количества разрядов (обязательно составить таблицу соответствия исходных слов и результирующих). Объяснить количество использованных бит." >> lnbk
        minipage (Just Top) (".38" <> textwidth) $ do
          vspace (Mm 1)
          tabular Nothing [VerticalLine, ParColumnTop "3cm", VerticalLine, ParColumnTop "2cm", VerticalLine] $ do
            hline >> (textbf "Код" & textbf "Буква") >> lnbk >> hline
            fixedWidthCodeRows Id.message
        minipage (Just Top) (".52" <> textwidth) $ do
          vspace (Mm 1)
          "Поскольку количество уникальных букв в последовательности равно 16, нам достаточно четырех бит\
          \ для записи каждого символа " >> (math . raw) "(2^4 = 16)."

fixedWidthCodeRows :: String -> LaTeXM ()
fixedWidthCodeRows msg = mapM_ row (fixedWidthCode msg)
  where
    row (a, b) = (fromString a & fromString [b]) >> lnbk >> hline

fixedWidthCode :: String -> [(String, Char)]
fixedWidthCode msg = zip codewords chars
  where
    codewords = [ a : b : c : [d] | a <- bits, b <- bits, c <- bits, d <- bits]
    bits = ['0', '1']
    chars = fst <$> (letterFrequency msg)

letterFrequencyRows :: String -> LaTeXM ()
letterFrequencyRows = (mapM_ row) . letterFrequency
  where
    row (char, freq) = (fromString [char] & (fromString . show) freq) >> lnbk >> hline

letterFrequency :: String -> [(Char, Int)]
letterFrequency msg = sortBy descending charFreqTuples
  where
    descending = (flip compare) `on` snd
    charFreqTuples = (\chars@(c:_) -> (c, length chars)) <$> ((group . sort) msg)
