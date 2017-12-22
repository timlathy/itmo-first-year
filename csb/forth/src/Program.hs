module Program where

import Data.List (find)

import qualified Vocabulary as V

writeProgram :: [String] -> String -> V.VocabularyList -> String
writeProgram constants code (V.VocabularyList internalWords) =
  "; === Constants\n" ++ declareConsts constants ++
  "\n; === Main program\nBEGIN:\n" ++ code ++
  "\n; === Internal subroutines\n" ++ (internalWords >>= V.code)
  where
    declareConsts = mconcat . (declareConst <$>) . (zip ([0..] :: [Int]))
    declareConst (cid, val) = "VAR" ++ show cid ++ ": WORD " ++ val ++ "\n"

pushConstant :: Int -> String
pushConstant cid =
  "; push constant\n\
  \JSR DECDSP\n\
  \CLA\n\
  \ADD VAR" ++ show cid ++ "\n" ++
  "MOV (DSP)\n"

setOrgAddr :: String -> String
setOrgAddr addr =
  "; org " ++ addr ++ "\n\
  \ORG " ++ addr ++ "\n"

insertVariable :: String -> String
insertVariable name =
  "; variable " ++ name ++ "\n\
  \VARIABLE_" ++ name ++ ": WORD 0000\n"

tryAsmWord :: String -> V.VocabularyList -> Maybe String
tryAsmWord w (V.VocabularyList asmWords) =
  V.code <$> find (((==) w) . V.word) asmWords
