import Chords
import Text.Read (readMaybe)
import NotesData
import Scales
import ScaleOrChordType
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    printWelcome
    selectOptions

selectOptions :: IO ()
selectOptions = do
    noteOption <- selectNoteOption
    semitoneOption <- selectSemitoneOption
    scaleOrChordOption <- selectScaleOrChordOption
    (putStr.show) noteOption
    (putStr.show) semitoneOption
    (putStrLn.show) scaleOrChordOption

selectNoteOption :: IO (BaseNote)
selectNoteOption = do
    putStrLn "Note:"
    mapM_ putStrLn baseNoteStrings
    choice <- getLine
    case readMaybe choice :: Maybe Int of
        Just n -> do
            return (allBaseNotes !! (n-1))
        Nothing -> do selectNoteOption
    where baseNoteStrings = optsAsNumberedList show allBaseNotes

selectSemitoneOption :: IO (Semitone)
selectSemitoneOption = do
    putStrLn "Semitone:"
    mapM_ putStrLn semiToneStrings
    choice <- getLine
    case readMaybe choice :: Maybe Int of
        Just n -> do
            return (semiTones !! (n-1))
        Nothing -> do selectSemitoneOption
    where semiTones = [Natural,Flat,Sharp]
          semiToneStrings = optsAsNumberedList stLongName semiTones

selectScaleOrChordOption :: IO (ScaleOrChordType)
selectScaleOrChordOption = do
    putStrLn "Scale or Chord:"
    mapM_ putStrLn scaleOrChordStrings
    choice <- getLine
    case readMaybe choice :: Maybe Int of
        Just n -> do
            return (scaleOrChords !! (n-1))
        Nothing -> do selectScaleOrChordOption
    where scaleOrChords = [
            Scale MAJOR,
            Chord TONIC,
            Chord FOURTH,
            Chord FIFTH,
            Chord D7]
          scaleOrChordStrings = optsAsNumberedList show scaleOrChords


printWelcome :: IO ()
printWelcome = mapM_ putStrLn [
    "===============================================================",
    "| Welcome to the Haskell Music Helper! Choose your options... |",
    "==============================================================="
    ]

optsAsNumberedList :: (t -> String) -> [t] -> [String]
optsAsNumberedList pf l = (zipWith (\a b -> show a ++ ". " ++ pf b) indexes l)

indexes :: [Int]
indexes = [1..]