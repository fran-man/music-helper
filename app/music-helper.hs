import Text.Read (readMaybe)
import NotesData
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to the Haskell Music Helper! Choose your options..."
    selectOptions

selectOptions :: IO ()
selectOptions = do
    noteOption <- selectNoteOption
    putStrLn (show noteOption)

selectNoteOption :: IO (BaseNote)
selectNoteOption = do
    putStrLn "Note:"
    mapM_ putStrLn baseNoteStrings
    choice <- getLine
    case readMaybe choice :: Maybe Int of
        Just n -> do
            return C_Base
        Nothing -> do selectNoteOption
        where baseNoteStrings = (zipWith (\a b -> show a ++ ". " ++ show b) indexes allBaseNotes)


indexes :: [Int]
indexes = [1..]

-- printForKey :: Note -> IO ()
-- printForKey n = do
--     print (majorScaleInKey n)
--     print (generateChordInKey n TONIC)
--     print (generateChordInKey n FOURTH)
--     print (generateChordInKey n FIFTH)
--     print (generateChordInKey n D7)
--     putStrLn (replicate 20 '=')