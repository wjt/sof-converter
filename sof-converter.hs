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
      Episode "20111002" "Recap"
        [ Segment "17" "0.00" "57.03.5"
        ]
    , Episode "20111009" "Chaos (with Niraj Lal)"
        [ Segment "17" "0.00" "57.07.5"
        ]
    , Episode "20111016" "Dystopias and Utopias"
        [ Segment "16" "57.45" "EOF"
        , Segment "17" "0.00" "56.57"
        ]
    , Episode "20111023" "Stress (with Djuke Veldhuis)"
        [ Segment "16" "57.46" "EOF"
        , Segment "17" "0.00" "56.58.5"
        ]
    , Episode "20111030" "Biotechnology (with Steve Frenk)"
        [ Segment "18" "0.15" "57.03"
        ]
    , Episode "20111106" "Four Fundamental Forces (with Michael Conterio)"
        [ Segment "17" "58.48.5" "EOF"
        , Segment "18" "0.00" "57.02.5"
        ]
    , Episode "20111113" "Environment (with Rachel Kennerley)"
        [ Segment "18" "3.00" "57.12"
        ]
    , Episode "20111120" "Music"
        [ Segment "18" "1.57" "EOF"
        ]
    , Episode "20111127" "Mathematics and Mathematicians (with Trevor Wood)"
        [ Segment "18" "4.34" "EOF"
        ]
    , Episode "20111204" "Cryptography (with James Grime)"
        [ Segment "18" "3.33.5" "EOF"
        , Segment "19" "0.00" "0.13.25"
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
    let result = concat [ "done/"
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

    renameFile glued desired
    return desired

url = "http://www.camfm.co.uk/index.php?\
      \option=com_radioshows&view=showpage&task=displayshowpage&id=104"

tagEpisode :: Episode -> Int -> Int -> FilePath -> IO ()
tagEpisode e i n f = do
    let args = [ "--artist", "The Science of Fiction"
               , "--album", "Season 2"
               , "--song", epTitle e
               , "--year", "2011"
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
