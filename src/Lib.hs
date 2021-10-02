module Lib
    ( mainEntry
    ) where

import ArgsParsing(parse)
import Sct(sct)
import SctConfig (generateConfig)
import System.Environment ( getArgs )

mainEntry :: IO ()
mainEntry = do
    (args, file:_) <- getArgs >>= parse
    let config = generateConfig args file
    withAFile file (sct config)

-- Apply function f on all lines of a file s and output the transformation
withAFile path func = putStr . unlines . func . lines =<< open path
  where
    open filename = if filename == "-" then getContents else readFile filename