module Inf.Lab4 where

import ReportBase

import Data.List (intersperse)
import Control.Monad (forM_, when)
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.AMSMath

writeReport :: IO ()
writeReport = renderFile "./renders/Inf-Lab4.tex" (execLaTeXM reportTeX)

type R = Int
type I = Int

data Message = Message Int [R] [I]

messages :: [Message]
messages = [ Message 36 [1, 0, 0] [0, 0, 1, 0]
           , Message 63 [0, 1, 0] [1, 1, 0, 0]
           , Message 90 [0, 1, 0] [1, 1, 1, 0]
           , Message 5 [0, 1, 1] [0, 0, 0, 0] ]

sumMod2 :: [Int] -> Int
sumMod2 = ((flip mod) 2) . sum

reportTeX :: LaTeXM ()
reportTeX = do
  baseHeader
  document $ do
    baseTitlePage ("Лабораторная работа №4", "Информатика", Just "Вариант 40", "2017 г.")
    sectionstar "Цель работы"
    "Овладеть навыками кодирования и декодирования сообщений с использованием самокорректирующихся кодов."
    sectionstar "Задания"
    enumerate $ do
      item Nothing <> do
        textit "Построить схему декодирования классического кода Хэмминга (7;4)." >> lnbk
        raw "\\begin{figure}[h]\\centering"
        includegraphics [IGWidth (Cm 18)] "../src/Inf/Lab4Schematics.pdf"
        raw "\\end{figure}"
      item Nothing <> do
        textit "Выписать последовательности 7-символьного кода, соответствующие варианту работы." >> lnbk >> newline
        borderedtable [(CenterColumn, 8)] $ do
          hline
          tfreerow $ fmap textbf [multirow 2 Nothing "№", "1", "2", "3", "4", "5", "6", "7"]
          trow $ fmap mt ["", "r_1", "r_2", "i_1", "r_3", "i_2", "i_3", "i_4"]
          forM_ messages (\(Message n rs is) -> trow $ fromIntegral <$>
            [n, rs !! 0, rs !! 1, is !! 0, rs !! 2, is !! 1, is !! 2, is !! 3])
      item Nothing <> do
        textit "Показать, имеются ли в принятых сообщениях ошибки, и если имеются, то какие. Написать правильные сообщения." >> lnbk >> newline
        "Рассмотрим синдромы " <> mt "s_1, s_2, s_3" <> " каждой последовательности, вычисляемые как:"
        flalignstar $ do
          raw "s_1 &= r_1 \\oplus i_1 \\oplus i_2 \\oplus i_4" >> lnbk
          raw "s_2 &= r_2 \\oplus i_1 \\oplus i_3 \\oplus i_4" >> lnbk
          raw "s_3 &= r_3 \\oplus i_2 \\oplus i_3 \\oplus i_4&"
        newpage
        forM_ messages (\(Message n rs is) -> do
          parbreak >> textbf ("Сообщение " <> (fromIntegral n)) >> newline
          analyze7BitMessage rs is >> newline)
      item Nothing <> do
        textit "С помощью кругов Эйлера объяснить построение классического кода Хэмминга (7;4)." >> lnbk >> newline
        raw "\\begin{figure}[h]\\centering" 
        includegraphics [IGWidth (Cm 13.6)] "../src/Inf/Lab4Diagram7b.pdf"
        raw "\\end{figure}"

analyze7BitMessage :: [R] -> [I] -> LaTeXM ()
analyze7BitMessage [r1, r2, r3] [i1, i2, i3, i4] = do
  minipage (Just Top) (".28" <> textwidth) $ do
    forM_ (zip ['1'..] equations) equationLine
  minipage (Just Top) (".62" <> textwidth) $ do
    "Десятичное значение синдромов последовательности, записанных последовательно (" <>
      (mconcat $ fromIntegral <$> sums) <> "),  равно " <> fromIntegral sumsDec
      <> ", что указывает на " <> if sumsDec == 0
        then "отсутствие ошибки."
        else "ошибку " <> locWithPrep sumsDec <> " бите, которым является "
          <> (bitLabels !! (sumsDec - 1)) <> "."
  when (sumsDec /= 0) $ do
    newline
    "Получим правильное сообщение, инвертировав ошибочный бит: "
    highlightErr sourceMessage <> raw " \\rightarrow$\\!$ " <> highlightErr correctedMessage
    where
      equationLine (n, eq) = mt ("s_" <> fromString [n] <> "=") <> " " <> eq <> newline
      equations = eqn <$> zip addends sums
      eqn (adds, s) = (math $ mconcat $
        (intersperse $ raw " \\oplus ") (fromIntegral <$> adds))
        <> " " <> (mt "=" <> " " <> fromIntegral s)
      addends = [ [r1, i1, i2, i4]
                , [r2, i1, i3, i4]
                , [r3, i2, i3, i4] ]
      sums = sumMod2 <$> addends
      sumsDec = bitsToDec sums
      bitLabels = mt <$> ["r_1", "r_2", "i_1", "r_3", "i_2", "i_3", "i_4"]
      sourceMessage = [r1, r2, i1, r3, i2, i3, i4]
      correctedMessage =
        let m = sourceMessage
            pos = sumsDec - 1
        in take pos m ++ complement (m !! pos) : drop (pos + 1) m
      highlightErr m =
        let mtext = fromIntegral <$> m
            pos = sumsDec - 1
        in mconcat $ take pos mtext ++ textbf (mtext !! pos) : drop (pos + 1) mtext
      complement 1 = 0
      complement 0 = 1

bitsToDec :: [Int] -> Int
bitsToDec = (foldr digitToPower 0) . (zip powers) . reverse
  where
    digitToPower (pow, d) = (+ d * 2^pow)
    powers :: [Int]
    powers = [0..]

locWithPrep :: Int -> LaTeXM ()
locWithPrep 1 = "в первом"
locWithPrep 2 = "во втором"
locWithPrep 3 = "в третьем"
locWithPrep 4 = "в четвертом"
locWithPrep 5 = "в пятом"
locWithPrep 6 = "в шестом"
locWithPrep 7 = "в седьмом"