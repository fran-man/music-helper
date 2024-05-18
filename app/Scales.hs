module Scales where

import NotesData
import MusicUtils

data ScaleType = MAJOR deriving Eq

instance Show ScaleType where
    show MAJOR = "Major"

majorScaleInKey :: Note -> [Note]
majorScaleInKey k = majorScale $ rotateOctaveToKey twoOctavesWithSharps k

majorScale :: [Note] -> [Note]
majorScale [] = []
majorScale n@(t:_) =  notesInScale n majorScalePattern ++ [t]

majorScalePattern :: [Bool]
majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]