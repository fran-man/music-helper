import NotesData

majorScaleInKey :: Note -> [Note]
majorScaleInKey k = majorScale $ rotateOctaveToKey octaveWithSharps k

majorScale :: [Note] -> [Note]
majorScale n@(t:ns) =  notesInScale n majorScalePattern ++ [t]

notesInScale :: [b] -> [Bool] -> [b]
notesInScale ns p = map fst $ filter (\(n,b) -> b == True) (combineListsAsPairs ns p)

combineListsAsPairs :: [a] -> [b] -> [(a,b)]
combineListsAsPairs l1 l2 = zipWith (\m n -> (m,n)) l1 l2

majorScalePattern :: [Bool]
majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]

rotateOctaveToKey :: Eq t => [t] -> t -> [t]
rotateOctaveToKey [] _ = []
rotateOctaveToKey [n] _ = [n]
rotateOctaveToKey n@(n1:ns) k
    | n1 == k = n
    | otherwise = rotateOctaveToKey (ns ++ [n1]) k

main :: IO ()
main = print (majorScaleInKey (Note C_Base Natural))