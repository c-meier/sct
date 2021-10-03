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
    } deriving (Show)

generateConfig :: [AP.Flag] -> String -> Config
generateConfig flags file = Config {langSpec = lang, only = AP.Only `elem` flags, wantedTag = wantedTag}
    where
        wantedTag = if AP.WantStudent `elem` flags then Student else Correction
        lang = divineLangSpec file
        ifset a v d = if a `elem` flags then v else d

divineLangSpec filename = getLangSpec (takeExtension filename)

getLangSpec :: String -> LangSpec
getLangSpec ".sctignore" = LangSpec "#" "##!" ""
getLangSpec ".xml" = LangSpec "<!--" "<!--!" "-->"
getLangSpec extension = LangSpec "//" "//!" ""
