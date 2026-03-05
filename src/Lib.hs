-- | Entry point module that wires together CLI parsing, config file loading,
-- and the core SCT transformation.
module Lib
    ( mainEntry
    ) where

-- "qualified" imports bring in a module under a prefix (e.g. T.Text, TIO.putStr)
-- to avoid name clashes with Prelude functions like 'lines' or 'putStr'.
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import ArgsParsing(parse)
import Sct(sct)
import SctConfig (generateConfig)
import SctConfigFile (loadConfigFiles)
import System.Environment ( getArgs )

-- | Main entry point of the application.
-- Uses 'do' notation to sequence IO actions:
--   1. Parse command-line arguments into flags and filenames
--   2. Load and merge TOML config files (XDG + project-local)
--   3. Build the final Config (file config + CLI flags + file extension)
--   4. Read the input file, apply the transformation, and print the result
mainEntry :: IO ()
mainEntry = do
    -- '>>=' (bind) pipes the result of getArgs into parse.
    -- Pattern match extracts the flag list and at least one filename.
    (args, file:_) <- getArgs >>= parse
    fileConfig <- loadConfigFiles
    -- 'let' introduces a pure (non-IO) binding inside a do block.
    let config = generateConfig fileConfig args file
    withAFile file (sct config)

-- | Read all lines from a file (or stdin if path is "-"), apply a
-- transformation function to them, and print the result to stdout.
--
-- The '.' operator composes functions right-to-left, so:
--   TIO.putStr . T.unlines . func . T.lines
-- means: split into lines -> apply func -> rejoin lines -> print.
--
-- '=<<' is reverse bind: it feeds the result of 'open path' (on the right)
-- into the composed function chain (on the left).
withAFile path func = TIO.putStr . T.unlines . func . T.lines =<< open path
  where
    open filename = if filename == "-" then TIO.getContents else TIO.readFile filename
