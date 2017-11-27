{-# LANGUAGE DeriveGeneric #-}

module Vocabulary where

import GHC.Generics
import Data.Yaml

-- (internal vocabulary, asm vocabulary)
type MainVocabularies = (VocabularyList, VocabularyList)

data VocabularyList = VocabularyList [WordEntry]
  deriving (Show, Generic)

data WordEntry = WordEntry { word :: String
                           , requires :: Maybe [String]
                           , code :: String }
  deriving (Show, Generic)

instance FromJSON VocabularyList
instance FromJSON WordEntry

type ParsedVocabularyList = Either ParseException VocabularyList

loadMainVocabs :: IO MainVocabularies
loadMainVocabs = (,) <$> loadVocabulary "internal" <*> loadVocabulary "asm"

loadVocabulary :: String -> IO VocabularyList
loadVocabulary name = do
  file <- decodeFileEither fileName :: IO ParsedVocabularyList
  case file of
    Right parsed -> return parsed
    Left err -> fail ("while loading " ++ name ++ " vocabulary: " ++ show err)
  where
    fileName = "vocab/" ++ name ++ ".yaml"
