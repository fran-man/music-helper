import Chords
import NotesData
import Scales

main :: IO ()
main = do
    putStrLn (replicate 20 '=')
    mapM_ printForKey [Note C_Base Natural, Note G_Base Natural]

printForKey :: Note -> IO ()
printForKey n = do
    print (majorScaleInKey n)
    print (tonicChordInKey n)
    print (fourthChordInKey n)
    putStrLn (replicate 20 '=')