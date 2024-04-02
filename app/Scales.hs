module Scales where

import NotesData
import MusicUtils

majorScaleInKey :: Note -> [Note]
majorScaleInKey k = majorScale $ rotateOctaveToKey twoOctavesWithSharps k

majorScale :: [Note] -> [Note]
majorScale n@(t:ns) =  notesInScale n majorScalePattern ++ [t]

majorScalePattern :: [Bool]
majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]