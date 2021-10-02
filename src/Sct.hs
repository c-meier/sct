module Sct
    ( sct
    , Zone(..)
    , tagLines
    , trim
    ) where

import ArgsParsing(Flag(..))
import SctConfig(Config(..), Tag(..))
import Data.List
import Data.Char
import Data.Maybe


-- | Apply student correction transformer on all lines
sct :: Config -> [String] -> [String]
sct config = removeZone . removeOtherZone . toggleZone . tagLines (cmdPrefix config)
  where
    removeZone      = map snd
    toggleZone      = ifcond (tag == Student) (map (toggleComment (lineComment config)))
    removeOtherZone = ifcond (only config) (filter (tagMatchZone tag . fst))
    tag             = wantedTag config


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
    | CommandZone Zone
    deriving (Eq, Ord, Show)

-- | Indicate if the line is a command and the next zone
lineSwitchInfo cmdPrefix (prevZone, _) line = case cmd of
                        Just "["  -> (CommandZone CorrectionZone, line)
                        Just "[-" -> (CommandZone StudentZone, line)
                        Just "-"  -> (CommandZone StudentZone, line)
                        Just "-]" -> (CommandZone AllZone, line)
                        Just "]"  -> (CommandZone AllZone, line)
                        _         -> case prevZone of
                          CommandZone zone -> (zone, line)
                          zone -> (zone, line)
    where
        cmd = stripPrefix cmdPrefix (trim line)

-- | Annotate each line with the Zone to which it belongs
tagLines cmdPrefix = tail . scanl (lineSwitchInfo cmdPrefix) (AllZone, "")