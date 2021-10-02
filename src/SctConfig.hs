module SctConfig
    ( Config(..)
    , generateConfig
    , divineCmdPrefix
    , Tag(..)
    ) where

import qualified ArgsParsing as AP
import System.FilePath

data Tag
    = Student
    | Correction
    deriving (Eq, Enum, Show, Bounded)

data Config = Config 
  { lineComment :: String 
  , cmdPrefix :: String
  , only :: Bool 
  , wantedTag :: Tag
  } deriving (Show)

generateConfig :: [AP.Flag] -> String -> Config
generateConfig flags file = Config {lineComment = lineComment, cmdPrefix = cmdPrefix, only = AP.Only `elem` flags, wantedTag = wantedTag}
    where
        wantedTag = if AP.WantStudent `elem` flags then Student else Correction
        (lineComment, cmdPrefix) = divineCmdPrefix (takeExtension file)
        ifset a v d = if a `elem` flags then v else d

divineCmdPrefix :: String -> (String, String)
divineCmdPrefix ".sctignore" = ("#", "##!")
divineCmdPrefix extension = ("//", "//!")