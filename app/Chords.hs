module Chords where

import NotesData
import MusicUtils

data ChordType = TONIC | FOURTH | FIFTH | D7 deriving Eq

tonicChordPattern :: [Bool]
tonicChordPattern = [True, False, False, False, True, False, False, True, False, False, False, False]

fourthChordPattern :: [Bool]
fourthChordPattern = [False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, False, False, False, False, False, False, False, False, False]

fifthChordPattern :: [Bool]
fifthChordPattern = [False, False, False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, False, False, False, False, False, False, False]

d7ChordPattern :: [Bool]
d7ChordPattern = [False, False, False, False, False, False, False, True, False, False, False, True, False, False, True, False, False, True, False, False, False, False, False, False]

generateChordInKey :: Note -> ChordType -> [Note]
generateChordInKey k chord = notesInScale notes chordPattern
                                  where notes = rotateOctaveToKey twoOctavesWithSharps k
                                        chordPattern = generateChordPattern chord

generateChordPattern :: ChordType -> [Bool]
generateChordPattern TONIC = tonicChordPattern
generateChordPattern FOURTH = fourthChordPattern
generateChordPattern FIFTH = fifthChordPattern
generateChordPattern D7 = d7ChordPattern