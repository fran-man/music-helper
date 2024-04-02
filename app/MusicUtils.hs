module MusicUtils where

rotateOctaveToKey :: Eq t => [t] -> t -> [t]
rotateOctaveToKey [] _ = []
rotateOctaveToKey [n] _ = [n]
rotateOctaveToKey n@(n1:ns) k
    | n1 == k = n
    | otherwise = rotateOctaveToKey (ns ++ [n1]) k

notesInScale :: [b] -> [Bool] -> [b]
notesInScale ns p = map fst $ filter (\(_,b) -> b == True) (combineListsAsPairs ns p)

combineListsAsPairs :: [a] -> [b] -> [(a,b)]
combineListsAsPairs l1 l2 = zipWith (\m n -> (m,n)) l1 l2