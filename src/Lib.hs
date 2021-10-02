module Lib
    ( mainEntry
    ) where

import ArgsParsing(parse)
import Sct(sct)
import System.Environment ( getArgs )

mainEntry :: IO ()
mainEntry = do
    (args, files) <- getArgs >>= parse
    mapM_ (sct args) files
