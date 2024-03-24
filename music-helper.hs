data BaseNote = A_Base | B_Base | C_Base | D_Base | E_Base | F_Base | G_Base deriving Eq

instance Show BaseNote where
    show (A_Base) = "A"
    show (B_Base) = "B"
    show (C_Base) = "C"
    show (D_Base) = "D"
    show (E_Base) = "E"
    show (F_Base) = "F"
    show (G_Base) = "G"

data Semitone = Sharp | Flat | Natural deriving Eq

instance Show Semitone where
    show Sharp = "#"
    show Flat = "b"
    show Natural = ""

data Note' = Note' BaseNote Semitone deriving Eq

instance Show Note' where
    show (Note' b s) = show b ++ show s

octaveWithSharps :: [Note'] = 
    [
        Note' C_Base Natural,
        Note' C_Base Sharp,
        Note' D_Base Natural,
        Note' D_Base Sharp,
        Note' E_Base Natural,
        Note' F_Base Natural,
        Note' F_Base Sharp,
        Note' G_Base Natural,
        Note' G_Base Sharp,
        Note' A_Base Natural,
        Note' A_Base Sharp,
        Note' B_Base Natural
    ]

majorScaleInKey :: Note' -> [Note']
majorScaleInKey k = majorScale $ rotateOctaveToKey octaveWithSharps k

majorScale :: [Note'] -> [Note']
majorScale n@(t:ns) =  notesInScale n majorScalePattern ++ [t]

notesInScale ns p = map fst $ filter (\(n,b) -> b == True) (combineListsAsPairs ns p)

combineListsAsPairs :: [a] -> [b] -> [(a,b)]
combineListsAsPairs l1 l2 = zipWith (\m n -> (m,n)) l1 l2

majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]

rotateOctaveToKey n@(n1:ns) k
    | n1 == k = n
    | otherwise = rotateOctaveToKey (ns ++ [n1]) k