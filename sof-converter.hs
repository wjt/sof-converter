{-# LANGUAGE TemplateHaskell #-}
-- Usage:
-- • Put all your raw episode files (yyyymmddhh.mp3) into a directory,
--   along with an episodes.json file;
-- • Run this in that directory. It will put temporary files into 'wip' and
--   finished items into 'done' below CWD.
import Control.Monad
import System.Process
import System.Exit
import System.Directory

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import qualified Data.Aeson.Types as AE
import Data.Aeson.TH
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import Data.Char (toLower)
import Data.Maybe (catMaybes)

type Date = String
type Hour = String
type Point = String

data Segment =
    Segment { segHour :: Hour
            , segStart :: Point
            , segEnd :: Point
            }
  deriving
    Show

$(deriveJSON (map toLower . drop 3) ''Segment)

data Episode =
    Episode { epNumber :: Integer
            , epDate :: Date
            , epTitle :: String
            , epSegments :: [Segment]
            }
  deriving
    Show

$(deriveJSON (map toLower . drop 2) ''Episode)

data Season =
    Season { seasonNumber :: String
           , seasonEpisodes :: [Episode]
           }
  deriving
    Show

$(deriveJSON (map toLower . drop 6) ''Season)

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

outDir :: FilePath
outDir = "done"

episodeBasename :: Episode
                -> FilePath
episodeBasename e =
    concat [ outDir
           , "/"
           , epDate e
           , " - "
           , epTitle e
           ]

episodeTargetName :: Episode -> FilePath
episodeTargetName e = episodeBasename e ++ ".mp3"

glueEpisode :: Episode -> [FilePath] -> IO FilePath
glueEpisode e chunks = do
    let desired = episodeTargetName e
        stupid = episodeBasename e ++ "_MP3WRAP.mp3"

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

tagEpisode :: String
           -> Episode
{-         -> Int -}
           -> FilePath
           -> IO ()
tagEpisode seasonNumber e {-n-} f = do
    let args = [ "--artist", "The Science of Fiction"
               , "--album", "Season " ++ seasonNumber
               , "--song", epTitle e
               , "--year", take 4 (epDate e)
               , "--comment", url
               , "--track", show (epNumber e)
            {- , "--total", show n -}
               , f
               ]

    systemOrDie "id3tag" args

processOne :: Season
           -> Episode
           -> IO FilePath
processOne season e = do
    chunks <- splitEpisode e
    glued <- glueEpisode e chunks
    tagEpisode (seasonNumber season) e glued
    return glued

process :: Season
        -> IO ()
process season = do
    let episodes = seasonEpisodes season
     {- n = length episodes -}

    ret <- forM episodes $ \e -> do
        let name = episodeTargetName e
        exists <- doesFileExist name
        if not exists
            then liftM Just $ processOne season e
            else do
                putStrLn $ "refusing to re-create " ++ name
                return Nothing

    mapM_ putStrLn (catMaybes ret)

main = do
    result <- fmap (parse json) $ C.readFile "episodes.json"
    case eitherResult result of
        Left e -> error $ "grr: " ++ e
        Right value -> case AE.parseEither parseJSON value of
            Left e -> error e
            Right episodes -> process episodes
