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
    {-
      Episode "20120212" "Errata"
        [ Segment "17" "2.15.5" "59.57"
        ]
    , Episode "20120219" "Hybrids (with Djuke Veldhuis)"
        [ Segment "17" "4.45" "60.0"
        ]
    , Episode "20120304" "Conspiracy"
        [ Segment "17" "3.15" "59.55"
        ]
    ,-}
      Episode "20120311" "Steampunk, Shams and Space Cowboys (with Kat Arney)"
        [ Segment "17" "2.49" "60.01"
        , Segment "18" "0.10" "0.21"
        ]
    ]

systemOrDie cmd args = do
    exitCode <- rawSystem cmd args
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure _ -> exitWith exitCode

splitOne :: Date -> Segment -> IO FilePath
splitOne date s = do
    let input = date ++ segHour s ++ ".mp3"
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
               , "--album", "Season 3"
               , "--song", epTitle e
               , "--year", "2012"
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
