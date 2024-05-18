module ScaleOrChordType where

import Chords (ChordType)
import Scales (ScaleType)

data ScaleOrChordType = Chord ChordType | Scale ScaleType deriving Eq

-- Implement the Show instance for ScaleOrChordType
-- This prevents the default show which results in things
-- like `Chord D7` being printed
instance Show ScaleOrChordType where
    show (Scale s) = show s
    show (Chord c) = show c