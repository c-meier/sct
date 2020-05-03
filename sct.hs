import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Char
--import qualified Data.Text as DT
import Control.Monad
import Text.Printf

main = do
    (args, files) <- getArgs >>= parse
--    when (Unbuffered `elem` args) $ hSetBuffering stdout NoBuffering
    mapM_ (sct args) files

-- Apply function f on every line of a file s and output the transformation
withAFile s f = putStr . unlines . f . lines =<< open s
  where
    open f = if f == "-" then getContents else readFile f

sct [] file = withAFile file id
sct as file = withAFile file (removeTag . ifonly . toggleZone . tagLines AllZone)
  where
    removeTag s  = map (snd) s
    toggleZone s = ifset Student (map toggleComment) s
    ifonly s     = ifset Only (filter (isNotZone zone) . filter (isNotZone SwitchZone)) s 
    zone         = if Student `elem` as then CorrectionZone else StudentZone
    ifset a f    = if a `elem` as then f else id

isNotZone zone (z, _) = z /= zone
trim = dropWhileEnd isSpace . dropWhile isSpace

toggleComment (CorrectionZone, line) = (CorrectionZone, "//" ++ line)
toggleComment (StudentZone, ('/':'/':line)) = (StudentZone, line)
toggleComment x = x

data Zone
    = CorrectionZone
    | StudentZone
    | AllZone
    | SwitchZone
    deriving (Eq, Ord, Enum, Show, Bounded)

lineSwitchInfo line = case cmd of
                        Just "["  -> (True, CorrectionZone)
                        Just "[-" -> (True, StudentZone)
                        Just "-"  -> (True, StudentZone)
                        Just "-]" -> (True, AllZone)
                        Just "]"  -> (True, AllZone)
                        Just _    -> (False, AllZone)
                        Nothing   -> (False, AllZone)
    where
        cmd = stripPrefix "//!" line

tagLines zone [] = []
tagLines zone (l:ls)
    | isSwitch = (SwitchZone, l) : tagLines newZone ls
    | otherwise = (zone, l) : tagLines zone ls
    where
      (isSwitch, newZone) = lineSwitchInfo (trim l)

printLineZone (CorrectionZone , l) = "CZ:" ++ l
printLineZone (StudentZone , l) = "SZ:" ++ l
printLineZone (AllZone , l) = "AZ:" ++ l
printLineZone (SwitchZone , l) = "TZ:" ++ l

--render Squeeze   = map head. groupBy (\x y -> all (all isSpace) [x,y])
--render Tabs      = map $ concatMap (\c -> if c == '\t' then "^I" else [c])
--render Invisible = map $ concatMap visible
--  where
--    visible c | c == '\t' || isPrint c = [c]
--              | otherwise              = init . tail . show $ c
--render _ = id
--
--numberLine      = printf "%6d  %s"
--numberAll s     = zipWith numberLine [(1 :: Integer)..] s
--numberSome s    = reverse . snd $ foldl' draw (1,[]) s
--  where
--    draw (n,acc) s
--            | all isSpace s = (n,   s : acc)
--            | otherwise     = (n+1, numberLine n s : acc)


data Flag
    = Only               -- -o --only
    | Student            -- -s --student
    | Correction         -- -c --correction
    | Version            -- -V --version
    | Help               -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
   [Option ['o'] ["only"]        (NoArg Only)
        "Do not print the other version in comments."
   ,Option ['s'] ["student"]     (NoArg Student)
        "Print the student version. Incompatible with --correction."
   ,Option ['c'] ["correction"]  (NoArg Correction)
        "Print the corrected version. Incompatible with --student."
   ,Option ['V'] ["version"]     (NoArg Version)
        "Show the version of the application."
   ,Option []    ["help"]        (NoArg Help)
        "Print this help message"
   ]

parse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else if Version `elem` args
                then do hPutStrLn stderr version
                        exitWith ExitSuccess
                else if Student `elem` args && Correction `elem` args
                    then do hPutStrLn stderr ("Error: Options -s and -c can't be used together\n" ++ 
                                              usageInfo header flags)
                            exitWith (ExitFailure 1)
                    else return (nub (concatMap set args), files)

    (_,_,errs)   -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: sct -s [-o] [file]\n" ++
                   "       sct [-co] [file]\n" ++
                   "       sct -V\n" ++
                   "       sct --help"
          version = "Student Correction Transformer (sct) v0.1"     
--          set Dollar = [Dollar, Invisible]
--          set Tabs   = [Tabs,   Invisible]
          set f      = [f]
