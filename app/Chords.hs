module Chords where

import NotesData
import MusicUtils

tonicChordPattern :: [Bool]
tonicChordPattern = [True, False, False, False, True, False, False, True, False, False, False, False]

fourthChordPattern :: [Bool]
fourthChordPattern = [False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, False, False, False, False, False, False, False, False, False]

fifthChordPattern :: [Bool]
fifthChordPattern = [False, False, False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, False, False, False, False, False, False, False]

d7ChordPattern :: [Bool]
d7ChordPattern = [False, False, False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, True, False, False, False, False, False, False]

tonicChordInKey :: Note -> [Note]
tonicChordInKey k = tonicChord $ rotateOctaveToKey twoOctavesWithSharps k

tonicChord :: [Note] -> [Note]
tonicChord n =  notesInScale n tonicChordPattern

fourthChordInKey :: Note -> [Note]
fourthChordInKey k = fourthChord $ rotateOctaveToKey twoOctavesWithSharps k

fourthChord :: [Note] -> [Note]
fourthChord n =  notesInScale n fourthChordPattern

fifthChordInKey :: Note -> [Note]
fifthChordInKey k = fifthChord $ rotateOctaveToKey twoOctavesWithSharps k

fifthChord :: [Note] -> [Note]
fifthChord n =  notesInScale n fifthChordPattern

d7ChordInKey :: Note -> [Note]
d7ChordInKey k = d7Chord $ rotateOctaveToKey twoOctavesWithSharps k

d7Chord :: [Note] -> [Note]
d7Chord n =  notesInScale n d7ChordPattern