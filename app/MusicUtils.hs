module MusicUtils where

rotateOctaveToKey :: Eq t => [t] -> t -> [t]
rotateOctaveToKey [] _ = []
rotateOctaveToKey [n] _ = [n]
rotateOctaveToKey n@(n1:ns) k
    | n1 == k = n
    | otherwise = rotateOctaveToKey (ns ++ [n1]) k

-- Take in all notes, and the pattern for the given scale
-- Combine these together so we have a list of pairs [Note, Bool]
-- Filter this list to where the second value is True (i.e. the note is in the scale)
-- Finally, simplify the list my removing all the booleans using map fst
notesInScale :: [b] -> [Bool] -> [b]
notesInScale ns p = map fst $ filter (\(_,b) -> b == True) (combineListsAsPairs ns p)

-- Takes two lists and combines them into a list of pairs.
-- Each element from the first list is paired with
-- the corresponding element from the second list.
combineListsAsPairs :: [a] -> [b] -> [(a,b)]
combineListsAsPairs l1 l2 = zipWith (\m n -> (m,n)) l1 l2