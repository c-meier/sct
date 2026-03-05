-- | TOML config file discovery, parsing, and loading.
--
-- This module handles the IO side of configuration: finding config files
-- on disk, reading them, parsing TOML, and merging them. It uses the
-- 'toml-parser' library for TOML decoding.
--
-- Config files are searched in two locations (merged in this order):
--   1. XDG config directory: e.g. ~/.config/sct/config.toml (Linux),
--      ~/Library/Application Support/sct/config.toml (macOS),
--      %APPDATA%/sct/config.toml (Windows)
--   2. Project-local: .sct.toml in the current working directory
--
-- Example TOML config:
--
-- @
-- formatter-space = true
-- tag = "student"
--
-- [languages.".jsx"]
-- comment-prefix = "//"
-- cmd-prefix = "//!"
-- comment-suffix = ""
-- @

{-# LANGUAGE OverloadedStrings #-}

module SctConfigFile
    ( loadConfigFiles
    , decodeFileConfig
    ) where

import SctConfig (FileConfig(..), LangSpec(..), Tag(..), emptyFileConfig, mergeFileConfigs)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- 'decode' parses a TOML Text and returns a Result (either Success or Failure).
-- 'Result' has two constructors: Success [warnings] value | Failure [errors].
import Toml (decode, Result(..))
-- 'FromValue' is a typeclass (like an interface) that defines how to convert
-- a TOML value into a Haskell type. We implement it for our types below.
-- 'parseTableFromValue' bridges table-level parsing into a FromValue instance.
-- 'optKey' / 'reqKey' extract optional/required keys from a TOML table.
import Toml.Schema (FromValue(..), parseTableFromValue, optKey, reqKey)
-- 'getXdgDirectory' finds platform-specific config directories.
-- 'XdgConfig' is the variant for config files (as opposed to data or cache).
import System.Directory (getXdgDirectory, XdgDirectory(..), doesFileExist)
-- 'try' catches exceptions and returns Either SomeException a,
-- so we can handle errors without crashing.
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, stderr)

-- | How to decode a Tag from TOML. The 'instance' keyword says:
-- "here is how FromValue works for the Tag type".
--
-- We first decode the TOML value as Text (using the existing FromValue Text
-- instance), then convert "student" → Student, anything else → Correction.
--
-- 'fmap' applies a function inside a context (here, the Matcher context).
-- Think of it as: "once you've decoded the Text, apply toTag to it".
instance FromValue Tag where
    fromValue = fmap toTag . fromValue
      where
        toTag :: T.Text -> Tag
        toTag t = case T.toLower t of
            "student"    -> Student
            _            -> Correction

-- | How to decode a LangSpec from a TOML table like:
--
-- @
-- comment-prefix = "//"
-- cmd-prefix = "//!"
-- comment-suffix = ""
-- @
--
-- 'parseTableFromValue' says "treat this TOML value as a table".
-- Inside, '<$>' and '<*>' are the applicative operators:
--   '<$>' applies a function to a value in a context (like fmap)
--   '<*>' chains multiple contextual values together
-- So this reads as: "build a LangSpec from three required keys".
instance FromValue LangSpec where
    fromValue = parseTableFromValue $
        LangSpec <$> reqKey "comment-prefix"
                 <*> reqKey "cmd-prefix"
                 <*> reqKey "comment-suffix"

-- | How to decode a FileConfig from a top-level TOML document.
-- Uses 'do' notation in the ParseTable monad to extract each key.
-- 'optKey' returns Maybe — Nothing if the key is absent, Just value if present.
-- This is what makes partial configs work: missing keys stay as Nothing.
instance FromValue FileConfig where
    fromValue = parseTableFromValue $ do
        fmtSpace  <- optKey "formatter-space"
        onlyFlag  <- optKey "only"
        tag       <- optKey "tag"
        ctx       <- optKey "context"
        -- "languages" is an optional table of tables, e.g.:
        --   [languages.".py"]
        --   comment-prefix = "#"
        --   ...
        -- If absent, default to an empty Map.
        -- 'fmap' transforms the Maybe (Map ...) into Map ...,
        -- replacing Nothing with Map.empty.
        langs     <- fmap (maybe Map.empty id) (optKey "languages")
        return FileConfig
            { fcFormatterSpace = fmtSpace
            , fcOnly           = onlyFlag
            , fcTag            = tag
            , fcContext        = ctx
            , fcLanguages      = langs
            }

-- | Pure function to decode a TOML text string into a FileConfig.
-- Returns Left with error messages on failure, Right with the config on success.
-- Exported for use in tests (no IO needed).
decodeFileConfig :: T.Text -> Either String FileConfig
decodeFileConfig tomlText = case decode tomlText of
    Success _warnings fc -> Right fc
    Failure errs         -> Left (unlines errs)

-- | Load and merge config files from both locations.
-- XDG config is loaded first (lower priority), then project-local (higher priority).
-- 'mergeFileConfigs' makes the second argument override the first.
loadConfigFiles :: IO FileConfig
loadConfigFiles = do
    xdgConfig   <- loadXdgConfig
    localConfig <- loadOneConfig ".sct.toml"
    return (mergeFileConfigs xdgConfig localConfig)

-- | Load the XDG config file (e.g. ~/.config/sct/config.toml).
-- Wrapped in 'try' to gracefully handle cases where the XDG directory
-- cannot be determined (e.g. missing HOME environment variable).
-- On any exception, silently returns an empty config.
loadXdgConfig :: IO FileConfig
loadXdgConfig = do
    result <- try $ do
        dir <- getXdgDirectory XdgConfig "sct"
        loadOneConfig (dir ++ "/config.toml")
    case result of
        Left e  -> do
            -- Type annotation tells the compiler which exception type to catch.
            -- SomeException is the catch-all exception type in Haskell.
            let _ = e :: SomeException
            return emptyFileConfig
        Right fc -> return fc

-- | Load a single config file if it exists.
-- Returns emptyFileConfig if the file doesn't exist, can't be read,
-- or contains invalid TOML. Warnings are printed to stderr but never crash.
loadOneConfig :: FilePath -> IO FileConfig
loadOneConfig path = do
    exists <- doesFileExist path
    if not exists
        then return emptyFileConfig
        else do
            -- 'try' converts IO exceptions into an Either value
            -- so we can handle them without crashing.
            result <- try (TIO.readFile path)
            case result of
                Left e -> do
                    let _ = e :: SomeException
                    hPutStrLn stderr ("sct: warning: could not read config file " ++ path ++ ": " ++ show e)
                    return emptyFileConfig
                Right content -> case decodeFileConfig content of
                    Left err -> do
                        hPutStrLn stderr ("sct: warning: could not parse config file " ++ path ++ ":\n" ++ err)
                        return emptyFileConfig
                    Right fc -> return fc
