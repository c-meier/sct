{-# LANGUAGE OverloadedStrings #-}

module SctConfig
    ( Config(..)
    , LangSpec(..)
    , Tag(..)
    , generateConfig
    , divineLangSpec
    ) where

import qualified ArgsParsing as AP
import System.FilePath
import qualified Data.Text as T
import Control.Arrow (Arrow(first))
import Data.Maybe (listToMaybe)

data Tag
    = Student
    | Correction
    deriving (Eq, Enum, Show, Bounded)

data LangSpec = LangSpec
    { commentPrefix :: T.Text 
    , cmdPrefix :: T.Text
    , commentSuffix :: T.Text
    } deriving (Show, Eq)

data Config = Config 
    { langSpec :: LangSpec
    , only :: Bool 
    , wantedTag :: Tag
    , context :: Maybe T.Text
    } deriving (Show)

generateConfig :: [AP.Flag] -> String -> Config
generateConfig flags file = Config {langSpec = lang, only = AP.Only `elem` flags, wantedTag = wantedTag, context = contextArg}
    where
        wantedTag = if AP.WantStudent `elem` flags then Student else Correction
        lang = divineLangSpec file
        contextArg = listToMaybe [ T.pack x | AP.Context  x <- flags ]

divineLangSpec filename = getLangSpec (takeExtension filename)

getLangSpec :: String -> LangSpec
getLangSpec ".sctignore" = LangSpec "#" "##!" ""
getLangSpec ".gitignore" = LangSpec "#" "##!" ""
getLangSpec ".py" = LangSpec "#" "##!" ""
getLangSpec ".yaml" = LangSpec "#" "##!" ""
getLangSpec ".yml" = getLangSpec ".yaml"
getLangSpec ".xml" = LangSpec "<!--" "<!--!" "-->"
getLangSpec ".sql" = LangSpec "--" "--!" ""
getLangSpec extension = LangSpec "//" "//!" ""
