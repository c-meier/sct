{-# LANGUAGE OverloadedStrings #-}

module Sct
    ( sct
    , Zone(..)
    , tagLines
    ) where

import ArgsParsing(Flag(..))
import SctConfig(Config(..), LangSpec(..), Tag(..))
import Data.List
import Data.Char
import qualified Data.Text as T
import Data.Maybe


-- | Apply student correction transformer on all lines
sct :: Config -> [T.Text] -> [T.Text]
sct config = removeZone . removeOtherZone . toggleZone . tagLines (langSpec config)
  where
    removeZone      = map snd
    toggleZone      = ifcond (tag == Student) (map (toggleComment (langSpec config)))
    removeOtherZone = ifcond (only config) (filter (tagMatchZone tag . fst))
    tag             = wantedTag config


ifcond :: Bool -> (a -> a) -> a -> a
ifcond a f = if a then f else id

tagMatchZone tag AllZone = True
tagMatchZone Student StudentZone = True
tagMatchZone Correction CorrectionZone = True
tagMatchZone tag zone = False

isNotZone zone (z, _) = z /= zone

-- | Comments CorrectionZone and uncomments StudentZone
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} (CorrectionZone, line) = (CorrectionZone, T.concat [cPrefix, line, cSuffix])
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} x@(StudentZone, line)
    = (StudentZone, (defaultToArg (T.stripSuffix cSuffix) . defaultToArg (T.stripPrefix cPrefix)) line)
    where
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
toggleComment _ x = x

data Zone
    = CorrectionZone
    | StudentZone
    | AllZone
    | CommandZone Zone
    deriving (Eq, Ord, Show)

-- | Indicate if the line is a command and the next zone
lineSwitchInfo LangSpec{cmdPrefix = cmdPrefix, commentSuffix = cSuffix} (prevZone, _) line = case cmd of
                        Just "["  -> (CommandZone CorrectionZone, line)
                        Just "[-" -> (CommandZone StudentZone, line)
                        Just "-"  -> (CommandZone StudentZone, line)
                        Just "-]" -> (CommandZone AllZone, line)
                        Just "]"  -> (CommandZone AllZone, line)
                        _         -> case prevZone of
                          CommandZone zone -> (zone, line)
                          zone -> (zone, line)
    where
        cmd = (T.stripPrefix cmdPrefix . T.strip . defaultToArg (T.stripSuffix cSuffix) . T.strip) line
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)

-- | Annotate each line with the Zone to which it belongs
tagLines :: LangSpec -> [T.Text] -> [(Zone, T.Text)]
tagLines lang = tail . scanl (lineSwitchInfo lang) (AllZone, T.pack "")