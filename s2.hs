{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data

type Date = String
type Hour = String
type Point = String

data Episode =
    Episode { epDate :: Date
            , epTitle :: String
            , epSegments :: [Segment]
            }
  deriving
    (Show, Typeable, Data)

data Segment =
    Segment { segHour :: Hour
            , segStart :: Point
            , segEnd :: Point
            }
  deriving
    (Show, Typeable, Data)

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
