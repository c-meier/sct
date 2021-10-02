module Sct 
    ( sct
    ) where

import System.FilePath
import ArgsParsing(Flag(..))
import Data.List
import Data.Char
import Data.Maybe

-- Apply function f on every line of a file s and output the transformation
withAFile s f = putStr . unlines . f . lines =<< open s
  where
    open f = if f == "-" then getContents else readFile f

sct :: [Flag] -> [Char] -> IO ()
sct [] file = withAFile file id
sct flags file = withAFile file (removeTag . ifonly . toggleZone . tagLines cmdPrefix AllZone)
  where
    removeTag s  = map snd s
    toggleZone s = ifset Student (map (toggleComment lineComment)) s
    ifonly s     = ifset Only (filter (isNotZone zone) . filter (isNotZone SwitchZone)) s
    zone         = if Student `elem` flags then CorrectionZone else StudentZone
    ifset a f    = if a `elem` flags then f else id
    (lineComment, cmdPrefix) = divineCmdPrefix (takeExtension file)

divineCmdPrefix :: String -> (String, String)
divineCmdPrefix ".sctignore" = ("#", "##!")
divineCmdPrefix extension = ("//", "//!")

isNotZone zone (z, _) = z /= zone
trim = dropWhileEnd isSpace . dropWhile isSpace

toggleComment lineComment (CorrectionZone, line) = (CorrectionZone, lineComment ++ line)
toggleComment lineComment x@(StudentZone, line)
    = (StudentZone, fromMaybe line (stripPrefix lineComment line))
toggleComment _ x = x

data Zone
    = CorrectionZone
    | StudentZone
    | AllZone
    | SwitchZone
    deriving (Eq, Ord, Enum, Show, Bounded)

lineSwitchInfo cmdPrefix line = case cmd of
                        Just "["  -> (True, CorrectionZone)
                        Just "[-" -> (True, StudentZone)
                        Just "-"  -> (True, StudentZone)
                        Just "-]" -> (True, AllZone)
                        Just "]"  -> (True, AllZone)
                        Just _    -> (False, AllZone)
                        Nothing   -> (False, AllZone)
    where
        cmd = stripPrefix cmdPrefix line

tagLines :: String -> Zone -> [String] -> [(Zone, String)]
tagLines cmdPrefix zone [] = []
tagLines cmdPrefix zone (l:ls)
    | isSwitch = (SwitchZone, l) : tagLines cmdPrefix newZone ls
    | otherwise = (zone, l) : tagLines cmdPrefix zone ls
    where
      (isSwitch, newZone) = lineSwitchInfo cmdPrefix (trim l)