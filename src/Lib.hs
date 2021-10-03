module Lib
    ( mainEntry
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
withAFile path func = TIO.putStr . T.unlines . func . T.lines =<< open path
  where
    open filename = if filename == "-" then TIO.getContents else TIO.readFile filename