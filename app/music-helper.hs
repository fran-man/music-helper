import NotesData

majorScaleInKey :: Note -> [Note]
majorScaleInKey k = majorScale $ rotateOctaveToKey twoOctavesWithSharps k

majorScale :: [Note] -> [Note]
majorScale n@(t:ns) =  notesInScale n majorScalePattern ++ [t]

tonicChordInKey :: Note -> [Note]
tonicChordInKey k = tonicChord $ rotateOctaveToKey twoOctavesWithSharps k

tonicChord :: [Note] -> [Note]
tonicChord n =  notesInScale n tonicChordPattern

fourthChordInKey :: Note -> [Note]
fourthChordInKey k = fourthChord $ rotateOctaveToKey twoOctavesWithSharps k

fourthChord :: [Note] -> [Note]
fourthChord n =  notesInScale n fourthChordPattern

notesInScale :: [b] -> [Bool] -> [b]
notesInScale ns p = map fst $ filter (\(_,b) -> b == True) (combineListsAsPairs ns p)

combineListsAsPairs :: [a] -> [b] -> [(a,b)]
combineListsAsPairs l1 l2 = zipWith (\m n -> (m,n)) l1 l2

majorScalePattern :: [Bool]
majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]

tonicChordPattern :: [Bool]
tonicChordPattern = [True, False, False, False, True, False, False, True, False, False, False, False]

fourthChordPattern :: [Bool]
fourthChordPattern = [False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, False, False, False, False, False, False, False, False, False]

rotateOctaveToKey :: Eq t => [t] -> t -> [t]
rotateOctaveToKey [] _ = []
rotateOctaveToKey [n] _ = [n]
rotateOctaveToKey n@(n1:ns) k
    | n1 == k = n
    | otherwise = rotateOctaveToKey (ns ++ [n1]) k

main :: IO ()
main = do
    print (majorScaleInKey (Note C_Base Natural))
    print (tonicChordInKey (Note C_Base Natural))
    print (fourthChordInKey (Note C_Base Natural))