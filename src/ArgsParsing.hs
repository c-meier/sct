-- | Command-line argument parsing using Haskell's 'GetOpt' library.
-- Defines all CLI flags and validates incompatible option combinations.
{-# LANGUAGE DeriveDataTypeable #-}
module ArgsParsing
    -- The (..) syntax exports the type AND all its constructors,
    -- so callers can pattern match on Only, WantStudent, etc.
    ( Flag (..)
    , parse
    ) where

import System.Console.GetOpt
import System.IO
import System.Exit
import Data.List
import Data.Version(showVersion)
-- Auto-generated module by Cabal that provides the package version
-- from the .cabal file (e.g. "0.4.0.0").
import Paths_student_correction_transformer (version)


-- | All possible command-line flags.
-- This is a "sum type" (like an enum in other languages): a Flag value
-- is exactly ONE of these constructors.
-- 'Context' carries a String payload (the context name).
data Flag
    = Only        -- -o --only
    | WantStudent     -- -s --student
    | WantCorrection  -- -c --correction
    | Context String  -- -t --context
    | FormatterSpace  -- --formatter-space
    | Version     -- -V --version
    | Help        -- --help
    deriving (Eq,Ord,Show)

-- | Definition of all CLI options using GetOpt's 'Option' type.
-- Each Option specifies: short flags, long flags, argument spec, and help text.
-- 'NoArg X' means the flag takes no argument and produces value X.
-- 'ReqArg Constructor "METAVAR"' means the flag requires an argument,
-- which is passed to the Constructor (here 'Context').
flags =
   [Option ['o'] ["only"]        (NoArg Only)
        "Do not print the other version in comments."
   ,Option ['s'] ["student"]     (NoArg WantStudent)
        "Print the student version. Incompatible with --correction."
   ,Option ['c'] ["correction"]  (NoArg WantCorrection)
        "Print the corrected version. Incompatible with --student."
   ,Option ['t'] ["context"]         (ReqArg Context "CONTEXT")
        "Context to be shown."
   ,Option []    ["formatter-space"] (NoArg FormatterSpace)
        "Strip one optional space after the comment prefix when uncommenting."
   ,Option ['V'] ["version"]     (NoArg Version)
        "Show the version of the application."
   ,Option []    ["help"]        (NoArg Help)
        "Print this help message"
   ]

-- | Parse command-line arguments into a list of flags and a list of filenames.
-- Returns an IO action because it may exit the program (on --help, --version,
-- or errors). If no files are given, defaults to "-" (stdin).
parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
    -- Successful parse: opts is the list of flags, fs is remaining arguments.
    (opts,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        parseOpts opts files

    -- Parse error: errs contains error messages.
    (_,_,errs)   -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    -- 'where' introduces local definitions scoped to the enclosing function.
    where header = "Usage: sct -s [-o] [--formatter-space] [-t CONTEXT] [file]\n" ++
                   "       sct [-co] [--formatter-space] [-t CONTEXT] [file]\n" ++
                   "       sct -V\n" ++
                   "       sct --help"
          textversion = "Student Correction Transformer (sct) " ++ showVersion version
          set f      = [f]
          -- Guards (the '|' lines) are checked top-to-bottom;
          -- the first one that evaluates to True is taken.
          parseOpts opts files
            | Help `elem` opts =
                do hPutStrLn stderr (usageInfo header flags)
                   exitSuccess
            | Version `elem` opts =
                do hPutStrLn stderr textversion
                   exitSuccess
            | WantStudent `elem` opts && WantCorrection `elem` opts =
                do hPutStrLn stderr ("Error: Options -s and -c can't be used together\n" ++
                                                        usageInfo header flags)
                   exitWith (ExitFailure 1)
            -- 'nub' removes duplicates from the list.
            | otherwise = return (nub (concatMap set opts), files)
