module Sct
    ( sct
    ) where

import ArgsParsing(Flag(..))
import SctConfig(Config(..), Tag(..))
import Data.List
import Data.Char
import Data.Maybe


-- | Apply student correction transformer on all lines
sct :: Config -> [String] -> [String]
sct config = removeZone . removeOtherZone . toggleZone . tagLines (cmdPrefix config) AllZone
  where
    removeZone s      = map snd s
    toggleZone s      = ifcond (tag == Student) (map (toggleComment (lineComment config))) s
    removeOtherZone s = ifcond (only config) (filter (tagMatchZone tag . fst)) s
    tag               = wantedTag config


ifcond :: Bool -> (a -> a) -> a -> a
ifcond a f = if a then f else id

tagMatchZone tag AllZone = True
tagMatchZone Student StudentZone = True
tagMatchZone Correction CorrectionZone = True
tagMatchZone tag zone = False

isNotZone zone (z, _) = z /= zone

-- | Remove white space at the beginning and the end of a string
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Comments CorrectionZone and uncomments StudentZone
toggleComment lineComment (CorrectionZone, line) = (CorrectionZone, lineComment ++ line)
toggleComment lineComment x@(StudentZone, line)
    = (StudentZone, fromMaybe line (stripPrefix lineComment line))
toggleComment _ x = x

data Zone
    = CorrectionZone
    | StudentZone
    | AllZone
    | CommandZone
    deriving (Eq, Ord, Enum, Show, Bounded)

-- | Indicate if the line is a command and the next zone
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

-- | Annotate each line with the Zone to which it belongs
tagLines :: String -> Zone -> [String] -> [(Zone, String)]
tagLines cmdPrefix zone [] = []
tagLines cmdPrefix zone (l:ls)
    | isSwitch = (CommandZone, l) : tagLines cmdPrefix newZone ls
    | otherwise = (zone, l) : tagLines cmdPrefix zone ls
    where
      (isSwitch, newZone) = lineSwitchInfo cmdPrefix (trim l)