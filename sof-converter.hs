-- Usage:
-- • Put all your raw episode files (yyyymmddhh.mp3) into a directory;
-- • Update 'episodes' and 'tagEpisode';
-- • Run this in that directory. It will put temporary files into 'wip' and
--   finished items into 'done' below CWD.
import Control.Monad
import System.Process
import System.Exit
import System.Directory

type Date = String
type Hour = String
type Point = String

data Episode =
    Episode { epDate :: Date
            , epTitle :: String
            , epSegments :: [Segment]
            }
  deriving
    Show

data Segment =
    Segment { segHour :: Hour
            , segStart :: Point
            , segEnd :: Point
            }
  deriving
    Show

episodes :: [Episode]
episodes =
    [
      Episode "20110501" "Comic Book Science (with Ben Valsler)"
        [ Segment "ThinkRadio Ep1a" "58.06" "EOF"
        , Segment "ThinkRadio Ep1"   "0.04" "56.57"
        ]
    , Episode "20110508" "Doctor Who (with Michael Conterio)"
        [ Segment "ThinkRadio Ep2a" "58.05" "59.59"
        , Segment "ThinkRadio Ep2"   "0.06" "57.05"
        ]
    , Episode "20110515" "Computers (with Will Thompson)"
        [ Segment "ThinkRadio Ep3a" "58.00" "EOF"
        , Segment "ThinkRadio Ep3"   "0.00" "57.36"
        ]
    , Episode "20110522" "Fictional Scientist Grudge Match (with Will Thompson)"
        [ Segment "ThinkRadio Ep4a" "58.25" "60.06"
        , Segment "ThinkRadio Ep4"   "0.04" "59.01"
        ]
    , Episode "20110529" "Zombies (with Frank Swain and Thomas Wooley)" 
        [ Segment "ThinkRadio - Zombies a" "59.52" "60.03"
        , Segment "ThinkRadio - Zombies"   "0.00"  "58.43"
        ]
    , Episode "20110612" "Time Travel (with Andy Pontzen)"
        [ Segment "ThinkRadio - Time Travel - Andy Ponztena" "57.29" "60.04"
        , Segment "ThinkRadio - Time Travel - Andy Ponzten"  "0.00"  "59.04"
        ]
    , Episode "20110703" "Journey into the Fourth Dimension (with Prof. Andy Parker)"
        [ Segment "ThinkRadio - Prof Andy Parker" "0.00" "60.00"
        ]
    , Episode "20110710" "Late Victorian Gothic Literature (with Dorée Carrier)"
        [ Segment "ThinkRadio - Gothic Literatue - Doree-a" "58.12" "60.03"
        , Segment "ThinkRadio - Gothic Literatue - Doree" "0.03" "59.43"
        ]
    , Episode "20110724" "Memory (with Dr Amy Milton)"
        [ Segment "memorya" "57.07" "60.00"
        , Segment "memory"  "0.00"  "57.00"
        ]
    ]

systemOrDie cmd args = do
    exitCode <- rawSystem cmd args
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _ -> exitWith exitCode

splitOne :: Date -> Segment -> IO FilePath
splitOne _date s = do
    let input = segHour s ++ ".mp3"
        outputPattern = "wip/@f"
        output = "wip/" ++ input

        args = [ "-q"
               , input, segStart s, segEnd s
               , "-o", outputPattern
               ]

    systemOrDie "mp3splt" args
    return output

splitEpisode :: Episode -> IO [FilePath]
splitEpisode e = mapM (splitOne (epDate e)) (epSegments e)

glueEpisode :: Episode -> [FilePath] -> IO FilePath
glueEpisode e chunks = do
    let outDir = "done"
    let result = concat [ outDir
                        , "/"
                        , epDate e
                        , " - "
                        , epTitle e
                        ]
        desired = result ++ ".mp3"
        stupid = result ++ "_MP3WRAP.mp3"

    glued <- case chunks of
        []  -> error $ show e
        [f] -> return f
        _   -> do
            systemOrDie "mp3wrap" (desired:chunks)
            return stupid

    createDirectoryIfMissing False outDir
    renameFile glued desired
    return desired

url = "http://www.scienceoffiction.co.uk/"

tagEpisode :: Episode -> Int -> Int -> FilePath -> IO ()
tagEpisode e i n f = do
    let args = [ "--artist", "The Science of Fiction"
               , "--album", "Season 1"
               , "--song", epTitle e
               , "--year", take 4 (epDate e)
               , "--comment", url
               , "--track", show i
               , "--total", show n
               , f
               ]

    systemOrDie "id3tag" args

main = do
    let n = length episodes
    ret <- forM (zip [1..] episodes) $ \(i, e) -> do
        chunks <- splitEpisode e
        glued <- glueEpisode e chunks
        tagEpisode e i n glued
        return glued

    mapM_ putStrLn ret
