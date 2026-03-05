{-# LANGUAGE OverloadedStrings #-}

module SctConfigFile
    ( loadConfigFiles
    , decodeFileConfig
    ) where

import SctConfig (FileConfig(..), LangSpec(..), Tag(..), emptyFileConfig, mergeFileConfigs)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Toml (decode, Result(..))
import Toml.Schema (FromValue(..), parseTableFromValue, optKey, reqKey)
import System.Directory (getXdgDirectory, XdgDirectory(..), doesFileExist)
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, stderr)

-- | FromValue instance for Tag
instance FromValue Tag where
    fromValue = fmap toTag . fromValue
      where
        toTag :: T.Text -> Tag
        toTag t = case T.toLower t of
            "student"    -> Student
            _            -> Correction

-- | FromValue instance for LangSpec
instance FromValue LangSpec where
    fromValue = parseTableFromValue $
        LangSpec <$> reqKey "comment-prefix"
                 <*> reqKey "cmd-prefix"
                 <*> reqKey "comment-suffix"

-- | FromValue instance for FileConfig
instance FromValue FileConfig where
    fromValue = parseTableFromValue $ do
        fmtSpace  <- optKey "formatter-space"
        onlyFlag  <- optKey "only"
        tag       <- optKey "tag"
        ctx       <- optKey "context"
        langs     <- fmap (maybe Map.empty id) (optKey "languages")
        return FileConfig
            { fcFormatterSpace = fmtSpace
            , fcOnly           = onlyFlag
            , fcTag            = tag
            , fcContext        = ctx
            , fcLanguages      = langs
            }

-- | Decode a TOML text into a FileConfig
decodeFileConfig :: T.Text -> Either String FileConfig
decodeFileConfig tomlText = case decode tomlText of
    Success _warnings fc -> Right fc
    Failure errs         -> Left (unlines errs)

-- | Load and merge config files from XDG config dir and project-local .sct.toml
loadConfigFiles :: IO FileConfig
loadConfigFiles = do
    xdgConfig   <- loadXdgConfig
    localConfig <- loadOneConfig ".sct.toml"
    return (mergeFileConfigs xdgConfig localConfig)

-- | Load the XDG config file
loadXdgConfig :: IO FileConfig
loadXdgConfig = do
    result <- try $ do
        dir <- getXdgDirectory XdgConfig "sct"
        loadOneConfig (dir ++ "/config.toml")
    case result of
        Left e  -> do
            let _ = e :: SomeException
            return emptyFileConfig
        Right fc -> return fc

-- | Load a single config file if it exists
loadOneConfig :: FilePath -> IO FileConfig
loadOneConfig path = do
    exists <- doesFileExist path
    if not exists
        then return emptyFileConfig
        else do
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
