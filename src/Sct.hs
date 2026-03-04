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
sct config inputLines = removeZone . removeOtherZone . toggleZone . removeOtherContext . tagLines lang $ inputLines
  where
    lang            = langSpec config
    tag             = wantedTag config
    fmtSpace        = formatterSpace config || hasFormatterSpaceDirective lang inputLines
    removeZone      = map snd
    toggleZone      = ifcond (tag == Student) (map (toggleComment lang fmtSpace))
    removeOtherZone = ifcond (only config) (filter (tagMatchZone tag . fst))
    removeOtherContext = filter (contextMatchZone (context config) . fst)


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
toggleComment :: LangSpec -> Bool -> (Zone, T.Text) -> (Zone, T.Text)
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} _ (CorrectionZone ts, line) = (CorrectionZone ts, T.concat [spaces, cPrefix, rest, cSuffix])
    where (spaces, rest) = T.span isSpace line
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} fmtSpace (StudentZone ts, line)
    = (StudentZone ts, T.append spaces (stripSuffix . stripSpace . stripPrefix $ rest))
    where
        (spaces, rest) = T.span isSpace line
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
        stripPrefix = defaultToArg (T.stripPrefix cPrefix)
        stripSuffix = defaultToArg (T.stripSuffix cSuffix)
        stripSpace  = if fmtSpace then defaultToArg (T.stripPrefix " ") else id
toggleComment _ _ x = x

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
            Just (T.stripPrefix "[-" -> Just _) -> CommandZone (StudentZone nextContexts)
            Just (T.stripPrefix "-]" -> Just _) -> CommandZone (AllZone nextContexts)
            Just (T.stripPrefix "["  -> Just _) -> CommandZone (CorrectionZone nextContexts)
            Just (T.stripPrefix "-"  -> Just _) -> CommandZone (StudentZone nextContexts)
            Just (T.stripPrefix "]"  -> Just _) -> CommandZone (AllZone nextContexts)
            Just (T.stripPrefix "set " -> Just _) -> CommandZone (case prevZone of -- Set directive
                ContextZone z -> z
                CommandZone z -> z
                z -> z)
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

-- | Check if any line contains a 'set formatter-space' directive
hasFormatterSpaceDirective :: LangSpec -> [T.Text] -> Bool
hasFormatterSpaceDirective LangSpec{cmdPrefix = prefix, commentSuffix = suffix} = any isDirective
    where
        isDirective line = extractCmd line == Just "formatter-space"
        extractCmd = fmap (head' . T.words) . (T.stripPrefix "set " =<<) . T.stripPrefix prefix . T.strip . defaultToArg (T.stripSuffix suffix) . T.strip
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
        head' (x:_) = x
        head' []    = T.empty