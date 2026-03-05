{-# LANGUAGE OverloadedStrings #-}

module SctConfig
    ( Config(..)
    , LangSpec(..)
    , Tag(..)
    , FileConfig(..)
    , emptyFileConfig
    , mergeFileConfigs
    , generateConfig
    , divineLangSpec
    ) where

import qualified ArgsParsing as AP
import System.FilePath
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, fromMaybe)

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
    , formatterSpace :: Bool
    } deriving (Show, Eq)

-- | Partial configuration loaded from TOML config files.
-- Fields are Maybe to allow layered merging.
data FileConfig = FileConfig
    { fcFormatterSpace :: Maybe Bool
    , fcOnly           :: Maybe Bool
    , fcTag            :: Maybe Tag
    , fcContext        :: Maybe T.Text
    , fcLanguages      :: Map.Map String LangSpec
    } deriving (Show, Eq)

-- | Empty file config with no overrides
emptyFileConfig :: FileConfig
emptyFileConfig = FileConfig Nothing Nothing Nothing Nothing Map.empty

-- | Merge two file configs. The second (right) argument overrides the first.
mergeFileConfigs :: FileConfig -> FileConfig -> FileConfig
mergeFileConfigs a b = FileConfig
    { fcFormatterSpace = fcFormatterSpace b <|> fcFormatterSpace a
    , fcOnly           = fcOnly b <|> fcOnly a
    , fcTag            = fcTag b <|> fcTag a
    , fcContext        = fcContext b <|> fcContext a
    , fcLanguages      = Map.union (fcLanguages b) (fcLanguages a)
    }
  where
    -- Local (<|>) to avoid importing Control.Applicative just for Maybe
    Nothing <|> y = y
    x       <|> _ = x

generateConfig :: FileConfig -> [AP.Flag] -> String -> Config
generateConfig fc flags file = Config
    { langSpec        = lang
    , only            = AP.Only `elem` flags || fromMaybe False (fcOnly fc)
    , wantedTag       = tag
    , context         = contextArg
    , formatterSpace  = AP.FormatterSpace `elem` flags || fromMaybe False (fcFormatterSpace fc)
    }
    where
        tag = if AP.WantStudent `elem` flags then Student
              else if AP.WantCorrection `elem` flags then Correction
              else fromMaybe Correction (fcTag fc)
        lang = case Map.lookup (takeExtension file) (fcLanguages fc) of
            Just ls -> ls
            Nothing -> divineLangSpec file
        contextArg = case [ T.pack x | AP.Context x <- flags ] of
            (x:_) -> Just x
            []    -> fcContext fc

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
