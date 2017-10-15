{-# LANGUAGE AllowAmbiguousTypes #-}

module Inf.Lab3 where

import Data.List
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Inf.Lab3Id as Id
import ReportBase

import Text.LaTeX.Packages.AMSMath
import qualified Text.LaTeX.Packages.Trees.Qtree as T

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
            raw "&" >> raw "p_{1..3} = " >> frac 3 27 >> quad >> raw "p_{1..3} * log_2 p_{1..3} \\approx -0.35" >> lnbk
            raw "&" >> raw "p_{4..9} = " >> frac 2 27 >> quad >> raw "p_{4..9} * log_2 p_{4..9} \\approx -0.28" >> lnbk
            raw "&" >> raw "p_{10..16} = " >> frac 1 27 >> quad >> raw "p_{10..16} * log_2 p_{10..16} \\approx -0.18" >> lnbreak (Mm 4)
            raw "&" >> raw "i \\approx 3.99" >> raw "&"
      item Nothing <> do
        textit "Записать полученную последовательность в одну строку, посчитать её размер, учитывая, что для хранения используется кодировка UTF-16, т.е. на 1 символ необходимо 2 байта." >> lnbreak (Mm 1) >> newline
        texttt (fromString Id.message) >> ", длина строки = 27, размер = " >> math "27 * 2 = 54" >> " байта, или 432 бита."
      item Nothing <> do
        textit " Используя двоичный код, составить код постоянной длины, с помощью которого можно закодировать исходное сообщение при использовании минимального количества разрядов (обязательно составить таблицу соответствия исходных слов и результирующих). Объяснить количество использованных бит." >> lnbk
        minipage (Just Top) (".38" <> textwidth) $ do
          vspace (Mm 1)
          tabular Nothing [VerticalLine, ParColumnTop "3cm", VerticalLine, ParColumnTop "2cm", VerticalLine] $ do
            hline >> (textbf "Буква" & textbf "Код") >> lnbk >> hline
            fixedWidthCodeRows Id.message
        minipage (Just Top) (".52" <> textwidth) $ do
          vspace (Mm 1)
          "Поскольку количество уникальных букв в последовательности равно 16, нам достаточно четырех бит\
          \ для записи каждого символа " >> (math . raw) "(2^4 = 16)."
      item Nothing <> do
        textit "Посчитать результирующий объём, коэффициент сжатия." >> lnbreak (Mm 1) >> newline
        "Результирующий объем равен 16 символам, умноженным на 4 бита, которыми кодируется каждый из них, то есть 64 битам." >> newline >> newline
        "Коэффициент сжатия определяется как отношение " >> textit "входного " >> "потока к " >> textit "выходному. "
        "Примем входной поток как исходное сообщение в кодировке UTF-16 " >> textit "(см. задание 3)," >> " тогда "
        math ("k = " >> (432 / 64) >> " = " >> (fromString . show) ((432 / 64) :: Float)) >> "."
      item Nothing <> do
        textit "Составить код Шеннона-Фано. При ветвлении использовать максимально равные вероятности (при альтернативе, т.е. при равных вероятностях 2 групп, разбивать текущую группу символов на 2 группы, содержащие одинаковое количество символов). Обязательно составить дерево, таблицу соответствия исходных слов и результирующих." >> lnbreak (Mm 1) >> newline
        let (latexTree, codeMap) = ((shannonFanoTree 27) . letterFrequency) Id.message
            codeRows = (flip mapM_) (Map.toList codeMap) (\(char, code) -> (fromString [char]) & (fromString code) >> lnbk >> hline)
        raw "\\resizebox{\\textwidth}{!}{" >> T.tree id latexTree >> raw "}" >> newline
        tabular Nothing [VerticalLine, ParColumnTop "3cm", VerticalLine, ParColumnTop "2cm", VerticalLine] $ do
          hline >> (textbf "Буква" & textbf "Код") >> lnbk >> hline
          codeRows

data LeafDirection = LLeft | LRight | LRoot;

shannonFanoTree :: Int -> [(Char, Int)] -> (T.Tree (LaTeXM ()), Map Char String)
shannonFanoTree msglen = node (LRoot, [], Map.empty)
  where
    prob freq = frac (fromIntegral freq) (fromIntegral msglen)
    code LLeft = "1"
    code LRight = "0"
    code LRoot = ""
    node (dir, codes, codemap) [(c, f)] = (T.Leaf caption, newCodemap)
      where
        newCodemap = Map.insert c (codes ++ code dir) codemap
        caption = (textbf $ "[" <> code dir <> "] ")
          <> (fromString [c]) <> lnbk <> math ("(" <> prob f <> ")")
    node (dir, codes, codemap) branch = (T.Node (Just caption) nodes, newCodemap)
      where
        (nodes, newCodemap) = case splitBranch branch of
          ([], []) -> ([], codemap)
          (lhs, []) -> let (lnode, lmap) = (node (LLeft, leafCodes, codemap) lhs)
                       in ([lnode], Map.union codemap lmap)
          (lhs, rhs) -> let (lnode, lmap) = (node (LLeft, leafCodes, codemap) lhs) 
                            (rnode, rmap) = (node (LRight, leafCodes, codemap) rhs)
                        in ([lnode, rnode], Map.union codemap (Map.union lmap rmap))
        leafCodes = codes ++ code dir
        codeCaption = case dir of
          LRoot -> textbf $ ""
          d -> textbf $ "[" <> code d <> "] "
        caption = codeCaption <> (fromString chars) <> lnbk <> math ("(" <> probs <> ")" <> " = " <> probsum)
        chars = fst <$> branch
        probsum = prob . sum . (snd <$>) $ branch
        probs = mconcat $ intersperse " + " ((prob . snd) <$> branch)

splitBranch :: [(a, Int)] -> ([(a, Int)], [(a, Int)])
splitBranch node = go [] node
  where
    go [] [l, r] = ([l], [r])
    go lefts [] = (lefts, [])
    go lefts (r:rights) =
      let nls = lefts ++ [r]
      in if sum (snd <$> nls) > sum (snd <$> rights)
         then (nls, rights) else (go nls rights)

fixedWidthCodeRows :: String -> LaTeXM ()
fixedWidthCodeRows msg = mapM_ row (fixedWidthCode msg)
  where
    row (c, code) = (fromString [c] & fromString code) >> lnbk >> hline

fixedWidthCode :: String -> [(Char, String)]
fixedWidthCode msg = zip chars codewords
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
