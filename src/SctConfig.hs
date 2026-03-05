-- | Configuration types and generation logic.
--
-- This module defines the core data types used throughout the application:
--
--   * 'LangSpec' — how comments look in a given language (prefix, command prefix, suffix)
--   * 'Tag' — whether we want the student or correction version
--   * 'Config' — the fully resolved runtime configuration
--   * 'FileConfig' — a partial configuration loaded from TOML files, with 'Maybe'
--     fields so that missing values can be filled in from other sources
--
-- Configuration is resolved in layers (later overrides earlier):
--   hardcoded defaults → XDG config file → project-local .sct.toml → CLI flags

{-# LANGUAGE OverloadedStrings #-}
-- OverloadedStrings lets us write "hello" and have it be a T.Text
-- instead of a String. Without this, we'd need to write (T.pack "hello").

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

-- 'qualified' import means we must use the AP prefix: AP.Flag, AP.Only, etc.
import qualified ArgsParsing as AP
-- 'takeExtension' extracts the file extension: takeExtension "foo.py" == ".py"
import System.FilePath
import qualified Data.Text as T
-- Map is a key-value dictionary, here mapping file extensions to LangSpecs.
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, fromMaybe)

-- | Which output version to produce.
data Tag
    = Student
    | Correction
    deriving (Eq, Enum, Show, Bounded)

-- | Language-specific comment syntax specification.
--
-- For example, Java uses:
--   LangSpec { commentPrefix = "//", cmdPrefix = "//!", commentSuffix = "" }
--
-- And XML uses:
--   LangSpec { commentPrefix = "<!--", cmdPrefix = "<!--!", commentSuffix = "-->" }
--
-- The 'cmdPrefix' is what marks SCT command lines (e.g. "//![", "//!-").
-- The 'commentSuffix' is appended when commenting out lines (needed for XML-style comments).
data LangSpec = LangSpec
    { commentPrefix :: T.Text
    , cmdPrefix :: T.Text
    , commentSuffix :: T.Text
    } deriving (Show, Eq)

-- | Fully resolved runtime configuration.
-- All fields have concrete values (no Maybe) — this is what the core
-- transformation function 'sct' receives.
data Config = Config
    { langSpec :: LangSpec        -- ^ Comment syntax for the target language
    , only :: Bool                -- ^ If True, omit the other version entirely
    , wantedTag :: Tag            -- ^ Which version to produce (Student or Correction)
    , context :: Maybe T.Text     -- ^ Optional context filter (e.g. "part1")
    , formatterSpace :: Bool      -- ^ Strip one space after comment prefix when uncommenting
    } deriving (Show, Eq)

-- | Partial configuration loaded from TOML config files.
-- Fields are 'Maybe' so that missing values don't override values from
-- earlier layers. For example, if the XDG config sets formatter-space = true
-- but the project-local .sct.toml doesn't mention it, the XDG value is kept.
--
-- 'fcLanguages' maps file extensions (e.g. ".jsx") to custom LangSpecs.
data FileConfig = FileConfig
    { fcFormatterSpace :: Maybe Bool
    , fcOnly           :: Maybe Bool
    , fcTag            :: Maybe Tag
    , fcContext        :: Maybe T.Text
    , fcLanguages      :: Map.Map String LangSpec
    } deriving (Show, Eq)

-- | A FileConfig with no overrides — all fields are Nothing/empty.
-- Used as the starting point for merging and as a fallback when no
-- config file is found.
emptyFileConfig :: FileConfig
emptyFileConfig = FileConfig Nothing Nothing Nothing Nothing Map.empty

-- | Merge two file configs. The second (right) argument overrides the first (left).
-- For scalar fields: if the right has a value, use it; otherwise keep the left.
-- For languages: right-side entries win for the same extension key,
-- but extensions only in the left are preserved (Map.union is left-biased,
-- so we put 'b' on the left to give it priority).
mergeFileConfigs :: FileConfig -> FileConfig -> FileConfig
mergeFileConfigs a b = FileConfig
    { fcFormatterSpace = fcFormatterSpace b <|> fcFormatterSpace a
    , fcOnly           = fcOnly b <|> fcOnly a
    , fcTag            = fcTag b <|> fcTag a
    , fcContext        = fcContext b <|> fcContext a
    , fcLanguages      = Map.union (fcLanguages b) (fcLanguages a)
    }
  where
    -- Local definition of (<|>) for Maybe: return the first Just value.
    -- This is the same as Control.Applicative.(<|>) for Maybe, but defined
    -- locally to avoid importing the whole module for one function.
    Nothing <|> y = y
    x       <|> _ = x

-- | Build a final 'Config' by combining:
--   1. A 'FileConfig' (merged from config files)
--   2. CLI flags (always win over file config)
--   3. The input filename (to determine language spec)
--
-- Priority for each field:
--   * langSpec: FileConfig languages map → hardcoded 'getLangSpec'
--   * only: CLI --only flag → file config → False
--   * tag: CLI -s/-c → file config → Correction (default)
--   * context: CLI -t → file config → Nothing
--   * formatterSpace: CLI --formatter-space → file config → False
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
              -- 'fromMaybe default maybeVal' returns the value inside Just,
              -- or 'default' if it's Nothing.
              else fromMaybe Correction (fcTag fc)
        -- Try the config file's language map first, fall back to hardcoded defaults.
        lang = case Map.lookup (takeExtension file) (fcLanguages fc) of
            Just ls -> ls
            Nothing -> divineLangSpec file
        -- List comprehension: extract Context values from the flags list.
        -- The pattern 'AP.Context x' only matches Context flags, skipping others.
        contextArg = case [ T.pack x | AP.Context x <- flags ] of
            (x:_) -> Just x
            []    -> fcContext fc

-- | Determine the language spec from a filename by looking at its extension.
divineLangSpec filename = getLangSpec (takeExtension filename)

-- | Hardcoded mapping from file extensions to language specs.
-- This is pattern matching on the function argument: each equation handles
-- one case. The last equation is the catch-all default (C/Java-style comments).
getLangSpec :: String -> LangSpec
getLangSpec ".sctignore" = LangSpec "#" "##!" ""
getLangSpec ".gitignore" = LangSpec "#" "##!" ""
getLangSpec ".py" = LangSpec "#" "##!" ""
getLangSpec ".yaml" = LangSpec "#" "##!" ""
getLangSpec ".yml" = getLangSpec ".yaml"
getLangSpec ".xml" = LangSpec "<!--" "<!--!" "-->"
getLangSpec ".sql" = LangSpec "--" "--!" ""
getLangSpec extension = LangSpec "//" "//!" ""
