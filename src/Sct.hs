{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
sct config = removeZone . removeOtherZone . toggleZone . removeOtherContext . tagLines (langSpec config)
  where
    removeZone      = map snd
    toggleZone      = ifcond (tag == Student) (map (toggleComment (langSpec config)))
    removeOtherZone = ifcond (only config) (filter (tagMatchZone tag . fst))
    removeOtherContext = filter (contextMatchZone (context config) . fst)
    tag             = wantedTag config


ifcond :: Bool -> (a -> a) -> a -> a
ifcond a f = if a then f else id

tagMatchZone :: Tag -> Zone -> Bool
tagMatchZone tag (AllZone _) = True
tagMatchZone Student (StudentZone _) = True
tagMatchZone Correction (CorrectionZone _) = True
tagMatchZone tag zone = False

contextInContexts :: T.Text -> [[T.Text ]] -> Bool
contextInContexts c cs = c `elem` concat cs

contextMatchZone :: Maybe T.Text -> Zone -> Bool
contextMatchZone _ (AllZone []) = True
contextMatchZone _ (StudentZone []) = True
contextMatchZone _ (CorrectionZone []) = True
contextMatchZone (Just c) (AllZone cs) = contextInContexts c cs
contextMatchZone (Just c) (StudentZone cs) = contextInContexts c cs
contextMatchZone (Just c) (CorrectionZone cs) = contextInContexts c cs
contextMatchZone mc (CommandZone zone) = contextMatchZone mc zone
contextMatchZone _ zone = False

isNotZone zone (z, _) = z /= zone

-- | Comments CorrectionZone and uncomments StudentZone
toggleComment :: LangSpec -> (Zone, T.Text) -> (Zone, T.Text)
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} (CorrectionZone ts, line) = (CorrectionZone ts, T.concat [cPrefix, line, cSuffix])
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} x@(StudentZone ts, line)
    = (StudentZone ts, (defaultToArg (T.stripSuffix cSuffix) . defaultToArg (T.stripPrefix cPrefix)) line)
    where
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
toggleComment _ x = x

data Zone
    = CorrectionZone [[T.Text]]
    | StudentZone [[T.Text]]
    | AllZone [[T.Text]]
    | CommandZone Zone
    | ContextZone Zone
    deriving (Eq, Ord, Show)

-- | Indicate if the line is a command and the next zone
lineSwitchInfo :: LangSpec -> (Zone, b) -> T.Text -> (Zone, T.Text)
lineSwitchInfo LangSpec{cmdPrefix = cmdPrefix, commentSuffix = cSuffix} (prevZone, _) line = (zone, line)
    where
        cmd :: Maybe T.Text
        cmd = (T.stripPrefix cmdPrefix . T.strip . defaultToArg (T.stripSuffix cSuffix) . T.strip) line
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
        splitContexts :: T.Text -> [T.Text]
        splitContexts contextsText = filter (not . T.null) (T.splitOn " " contextsText)
        prevContexts z = case z of
            CorrectionZone cs -> cs
            StudentZone cs -> cs
            AllZone cs -> cs
            CommandZone z2 -> prevContexts z2
            ContextZone z2 -> prevContexts z2
        mergeContexts :: [T.Text] -> [[T.Text]] -> [[T.Text]]
        mergeContexts c cs = c : cs
        nextContexts = prevContexts prevZone
        contextZone :: Zone -> ([[T.Text]] -> [[T.Text]]) -> Zone
        contextZone z contextsFunc = case z of
            CorrectionZone cs -> ContextZone (CorrectionZone (contextsFunc cs))
            StudentZone cs -> ContextZone (StudentZone (contextsFunc cs))
            AllZone cs -> ContextZone (AllZone (contextsFunc cs))
            CommandZone z2 -> contextZone z2 contextsFunc
            ContextZone z2 -> contextZone z2 contextsFunc
        zone = case cmd of
            Just "["  -> CommandZone (CorrectionZone nextContexts)
            Just "[-" -> CommandZone (StudentZone nextContexts)
            Just "-"  -> CommandZone (StudentZone nextContexts)
            Just "-]" -> CommandZone (AllZone nextContexts)
            Just "]"  -> CommandZone (AllZone nextContexts)
            Just (T.stripPrefix "@ " -> Just contextsText) -> -- Tags command
                let contexts = splitContexts contextsText in contextZone prevZone (mergeContexts contexts)
            Just (T.stripPrefix "@" -> Just _) -> contextZone prevZone tail -- Tags end command
            _         -> case prevZone of
                ContextZone zone -> zone
                CommandZone zone -> zone
                zone -> zone

-- | Annotate each line with the Zone to which it belongs
tagLines :: LangSpec -> [T.Text] -> [(Zone, T.Text)]
tagLines lang = tail . scanl (lineSwitchInfo lang) (AllZone [], T.pack "")