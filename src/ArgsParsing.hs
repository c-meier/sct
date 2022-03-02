{-# LANGUAGE DeriveDataTypeable #-}
module ArgsParsing
    ( Flag (..)
    , parse
    ) where

import System.Console.GetOpt
import System.IO
import System.Exit
import Data.List
import Data.Version(showVersion)
import Paths_student_correction_transformer (version)


data Flag
    = Only        -- -o --only
    | WantStudent     -- -s --student
    | WantCorrection  -- -c --correction
    | Context String  -- -t --context
    | Version     -- -V --version
    | Help        -- --help
    deriving (Eq,Ord,Show)

flags =
   [Option ['o'] ["only"]        (NoArg Only)
        "Do not print the other version in comments."
   ,Option ['s'] ["student"]     (NoArg WantStudent)
        "Print the student version. Incompatible with --correction."
   ,Option ['c'] ["correction"]  (NoArg WantCorrection)
        "Print the corrected version. Incompatible with --student."
   ,Option ['t'] ["context"]         (ReqArg Context "CONTEXT")
        "Context to be shown."
   ,Option ['V'] ["version"]     (NoArg Version)
        "Show the version of the application."
   ,Option []    ["help"]        (NoArg Help)
        "Print this help message"
   ]

parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
    (opts,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        parseOpts opts files

    (_,_,errs)   -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: sct -s [-o] [-t CONTEXT] [file]\n" ++
                   "       sct [-co] [-t CONTEXT] [file]\n" ++
                   "       sct -V\n" ++
                   "       sct --help"
          textversion = "Student Correction Transformer (sct) " ++ showVersion version
          set f      = [f]
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
            | otherwise = return (nub (concatMap set opts), files)
