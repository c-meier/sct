-- | Core transformation logic for the Student Correction Transformer.
--
-- This module takes a source file containing both student and correction code
-- (delimited by special command comments) and produces either the student
-- version or the correction version.
--
-- == How it works
--
-- A source file is annotated with command comments that delimit zones:
--
-- @
-- public class Example {
--     \/\/![              ← start correction zone (teacher code visible, student code commented)
--     System.out.println("correct answer");
--     \/\/!-              ← switch to student zone
--     \/\/System.out.println("TODO: fill in");
--     \/\/!]              ← end zone, back to shared code
--     return result;
-- }
-- @
--
-- The transformation pipeline:
--   1. 'tagLines' — annotate each line with the Zone it belongs to
--   2. Filter by context (if @-t@ flag is set)
--   3. Toggle comments (if producing student version)
--   4. Filter zones (if @-o@ / only mode is set)
--   5. Strip zone annotations, returning plain text lines

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ViewPatterns #-}
-- ViewPatterns lets us call a function inside a pattern match.
-- For example: Just (T.stripPrefix "[" -> Just _)
-- means: "if the value is Just x, and T.stripPrefix "[" x succeeds, then match".
-- This avoids nested case expressions when parsing command strings.

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


-- | Apply the student correction transformer to all lines.
--
-- This is the main entry point. It composes several transformations
-- in a pipeline (read right-to-left with '.'):
--
--   tagLines → removeOtherContext → toggleZone → removeOtherZone → removeZone
--
-- Each step operates on a list of (Zone, Text) pairs, except the final
-- 'removeZone' which strips the zone annotations to produce plain [Text].
sct :: Config -> [T.Text] -> [T.Text]
sct config inputLines = removeZone . removeOtherZone . toggleZone . removeOtherContext . tagLines lang $ inputLines
  where
    lang            = langSpec config
    tag             = wantedTag config
    -- formatter-space is enabled if the CLI flag is set OR if the file
    -- contains a "//!set formatter-space" directive.
    fmtSpace        = formatterSpace config || hasFormatterSpaceDirective lang inputLines
    -- Strip zone annotations, keeping only the text.
    removeZone      = map snd
    -- If producing student version, comment correction lines and uncomment student lines.
    toggleZone      = ifcond (tag == Student) (map (toggleComment lang fmtSpace))
    -- If --only flag is set, remove lines belonging to the other zone.
    removeOtherZone = ifcond (only config) (filter (tagMatchZone tag . fst))
    -- Remove lines that don't match the requested context.
    removeOtherContext = filter (contextMatchZone (context config) . fst)


-- | Conditional function application.
-- If the condition is True, apply the function f; otherwise return the input unchanged.
-- 'id' is the identity function: id x == x.
ifcond :: Bool -> (a -> a) -> a -> a
ifcond a f = if a then f else id

-- | Check if a zone should be included for a given tag.
-- AllZone lines are always included.
-- StudentZone lines are included when producing the Student version.
-- CorrectionZone lines are included when producing the Correction version.
tagMatchZone :: Tag -> Zone -> Bool
tagMatchZone tag (AllZone _) = True
tagMatchZone Student (StudentZone _) = True
tagMatchZone Correction (CorrectionZone _) = True
tagMatchZone tag zone = False

-- | Check if a context string appears in any of the nested context lists.
-- Each zone carries a list of context groups ([[T.Text]]).
-- A line matches if the desired context appears in any group.
contextInContexts :: T.Text -> [[T.Text ]] -> Bool
contextInContexts c cs = c `elem` concat cs

-- | Check if a line's zone matches the requested context filter.
-- Rules:
--   * No contexts on the zone (empty list) → always include
--   * A context filter is set and matches → include
--   * CommandZone delegates to its inner zone
--   * Otherwise → exclude
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

-- | Toggle comments on a line depending on its zone.
--
-- When producing the student version:
--   * CorrectionZone lines get commented out (add comment prefix/suffix)
--   * StudentZone lines get uncommented (strip comment prefix/suffix)
--   * All other zones are left unchanged
--
-- Leading whitespace is preserved: "    //code" becomes "    code" (not "code"),
-- and "    code" becomes "    //code" (not "//    code").
--
-- The 'fmtSpace' parameter controls whether to also strip one extra space
-- after the comment prefix when uncommenting (e.g. "// code" → "code"
-- instead of " code").
toggleComment :: LangSpec -> Bool -> (Zone, T.Text) -> (Zone, T.Text)
-- Comment out a correction line: prepend the comment prefix after leading spaces.
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} _ (CorrectionZone ts, line) = (CorrectionZone ts, T.concat [spaces, cPrefix, rest, cSuffix])
    -- T.span isSpace splits "    hello" into ("    ", "hello").
    where (spaces, rest) = T.span isSpace line
-- Uncomment a student line: strip the comment prefix (and optionally one space).
toggleComment LangSpec{commentPrefix=cPrefix, commentSuffix=cSuffix} fmtSpace (StudentZone ts, line)
    = (StudentZone ts, T.append spaces (stripSuffix . stripSpace . stripPrefix $ rest))
    where
        (spaces, rest) = T.span isSpace line
        -- 'defaultToArg' tries to apply a Maybe-returning function.
        -- If the function returns Just (success), use the result.
        -- If it returns Nothing (the prefix/suffix wasn't found), keep the original.
        -- This makes stripping tolerant: if the comment prefix isn't there, no crash.
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
        stripPrefix = defaultToArg (T.stripPrefix cPrefix)
        stripSuffix = defaultToArg (T.stripSuffix cSuffix)
        stripSpace  = if fmtSpace then defaultToArg (T.stripPrefix " ") else id
-- Catch-all: lines in other zones (AllZone, CommandZone, etc.) are unchanged.
toggleComment _ _ x = x

-- | A Zone represents what kind of section a line belongs to.
--
-- The [[T.Text]] field on content zones holds stacked context groups:
-- e.g. [["part1", "part2"], ["part3"]] means this zone has two nested
-- context annotations.
--
-- 'CommandZone' wraps a zone to mark command lines (like "//![").
-- 'ContextZone' wraps a zone to mark context directive lines (like "//!@ part1").
data Zone
    = CorrectionZone [[T.Text]]  -- ^ Lines visible only in the correction version
    | StudentZone [[T.Text]]     -- ^ Lines visible only in the student version (commented out in source)
    | AllZone [[T.Text]]         -- ^ Lines visible in both versions
    | CommandZone Zone            -- ^ A command line (e.g. "//![", "//!-", "//!]")
    | ContextZone Zone            -- ^ A context directive line (e.g. "//!@ part1")
    deriving (Eq, Ord, Show)

-- | Determine the zone for a single line based on its content and the previous zone.
--
-- This is the state machine that drives zone transitions. It examines whether
-- the line starts with the command prefix (e.g. "//!") and which command follows:
--
--   * @[-@  → start student zone (closing bracket form)
--   * @-]@  → end student zone, return to shared
--   * @[@   → start correction zone
--   * @-@   → start student zone
--   * @]@   → end correction zone, return to shared
--   * @set@ → file-level directive (e.g. formatter-space), zone unchanged
--   * @@ @  → push a context filter
--   * @@    → pop the most recent context filter
--
-- Any text after the command character is discarded (e.g. "//![- some comment"
-- is treated the same as "//![-").
--
-- Uses ViewPatterns: @Just (T.stripPrefix "[-" -> Just _)@ means
-- "the command text starts with '[-', and we ignore the rest".
lineSwitchInfo :: LangSpec -> (Zone, b) -> T.Text -> (Zone, T.Text)
lineSwitchInfo LangSpec{cmdPrefix = cmdPrefix, commentSuffix = cSuffix} (prevZone, _) line = (zone, line)
    where
        -- Try to extract the command from the line.
        -- Steps (right to left, as '.' composes right-to-left):
        --   1. T.strip: remove surrounding whitespace
        --   2. defaultToArg (T.stripSuffix cSuffix): remove comment suffix if present
        --   3. T.strip: remove whitespace again (in case suffix had spacing)
        --   4. T.stripPrefix cmdPrefix: check if the line starts with the command prefix
        -- Result: Just "[-..." if it's a command, Nothing if it's a regular line.
        cmd :: Maybe T.Text
        cmd = (T.stripPrefix cmdPrefix . T.strip . defaultToArg (T.stripSuffix cSuffix) . T.strip) line
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)

        -- Split a space-separated context string into individual context names.
        splitContexts :: T.Text -> [T.Text]
        splitContexts contextsText = filter (not . T.null) (T.splitOn " " contextsText)

        -- Extract the context list from any zone, unwrapping CommandZone/ContextZone.
        prevContexts z = case z of
            CorrectionZone cs -> cs
            StudentZone cs -> cs
            AllZone cs -> cs
            CommandZone z2 -> prevContexts z2
            ContextZone z2 -> prevContexts z2

        -- Push a new context group onto the context stack.
        mergeContexts :: [T.Text] -> [[T.Text]] -> [[T.Text]]
        mergeContexts c cs = c : cs

        -- Carry forward contexts from the previous zone.
        nextContexts = prevContexts prevZone

        -- Wrap a zone with a ContextZone, applying a function to its context list.
        -- Used for "//!@ part1" (push) and "//!@" (pop).
        contextZone :: Zone -> ([[T.Text]] -> [[T.Text]]) -> Zone
        contextZone z contextsFunc = case z of
            CorrectionZone cs -> ContextZone (CorrectionZone (contextsFunc cs))
            StudentZone cs -> ContextZone (StudentZone (contextsFunc cs))
            AllZone cs -> ContextZone (AllZone (contextsFunc cs))
            CommandZone z2 -> contextZone z2 contextsFunc
            ContextZone z2 -> contextZone z2 contextsFunc

        -- The core state machine: match the command and determine the next zone.
        -- Order matters: "[-" must be checked before "[" (otherwise "[" would
        -- match first), and "-]" before "-".
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
            -- Not a command line: inherit the zone from the previous line,
            -- unwrapping any CommandZone or ContextZone wrappers.
            _         -> case prevZone of
                ContextZone zone -> zone
                CommandZone zone -> zone
                zone -> zone

-- | Annotate each line with the Zone it belongs to.
--
-- Uses 'scanl' to thread state (the current zone) through each line.
-- 'scanl' is like 'foldl' but keeps all intermediate results:
--   scanl f init [a, b, c] == [init, f init a, f (f init a) b, ...]
--
-- We seed it with a dummy initial value (AllZone [], "") and then drop
-- it with 'tail', so the output list has the same length as the input.
tagLines :: LangSpec -> [T.Text] -> [(Zone, T.Text)]
tagLines lang = tail . scanl (lineSwitchInfo lang) (AllZone [], T.pack "")

-- | Check if any line in the file contains a @set formatter-space@ directive.
-- This allows enabling formatter-space per-file without CLI flags.
--
-- The 'extractCmd' function is a pipeline (read right to left):
--   1. Strip surrounding whitespace
--   2. Optionally strip comment suffix (for XML-style comments)
--   3. Strip surrounding whitespace again
--   4. Strip the command prefix (e.g. "//!")
--   5. Strip "set " prefix
--   6. Take the first word (the directive name)
--
-- Each step returns Maybe, and '=<<' (bind for Maybe) short-circuits
-- on Nothing — so if any step fails, the whole pipeline returns Nothing.
hasFormatterSpaceDirective :: LangSpec -> [T.Text] -> Bool
hasFormatterSpaceDirective LangSpec{cmdPrefix = prefix, commentSuffix = suffix} = any isDirective
    where
        isDirective line = extractCmd line == Just "formatter-space"
        extractCmd = fmap (head' . T.words) . (T.stripPrefix "set " =<<) . T.stripPrefix prefix . T.strip . defaultToArg (T.stripSuffix suffix) . T.strip
        defaultToArg maybeFunc arg = fromMaybe arg (maybeFunc arg)
        -- Safe version of 'head' that returns empty text instead of crashing
        -- on an empty list. The standard 'head' is partial (crashes on []).
        head' (x:_) = x
        head' []    = T.empty
