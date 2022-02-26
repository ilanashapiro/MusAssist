module MusAssistAST 
(
  Chord (..)
)
where
--------------------------------------------------------------------------------
-- Data that is never used alone (they are used to create the tokens down below)
--------------------------------------------------------------------------------

data NoteName = 
    F
    | C
    | G 
    | D 
    | E 
    | A 
    | B
  deriving (Eq, Ord, Show) -- the notes are ordered in the order of sharps. reverse for order of flats

data Accidental = 
     Natural 
     | Sharp 
     | Flat
  deriving (Eq, Show)

type Octave = Int -- range is [1,8]

type Inversion = Int -- range is [1,4]

data Duration = 
     Whole 
     | DottedHalf 
     | Half 
     | DottedQuarter 
     | Quarter 
     | DottedEighth 
     | Eighth 
     | Sixteenth
  deriving (Eq, Show)

data Quality = 
     Major 
     | Minor
     | Augmented      -- chords only
     | Diminished     -- chords only
     | HalfDiminished -- seventh chords only
  deriving (Eq, Show)

data ChordType = 
     Triad 
     | Seventh
  deriving (Eq, Show)

-- | Custom collection of notes
data CustomChord = 
     EmptyChord 
     | ChordNotes Note CustomChord
  deriving (Eq, Show)

data NoteWithKey = Key Note Quality -- quality is major/minor ONLY
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Tokens that get translated from user input
--------------------------------------------------------------------------------
data Note = 
     Note NoteName Accidental Octave Duration 
     | Rest Duration
  deriving (Eq, Show)

-- | Predefined chords: these all happen in root position
data Chord = 
  Chord Note Quality ChordType Inversion -- note cannot be a rest
  | CustomChord
  deriving (Eq, Show)

data MusicState = 
  TimeSignature Int Duration -- number of beats, beat value 
  | KeySignature NoteName Quality
  | NewMeasure

-- all resulting chords in root position
data Cadence = 
     PerfAuth NoteWithKey
     | ImperfAuth NoteWithKey
     | Plagal NoteWithKey
     | HalfCad NoteWithKey
     | Deceptive NoteWithKey
  deriving (Eq, Show)

-- all resulting chords in root position
data HarmonicSequence = 
     AscFifths NoteWithKey Int |
     DescFifths NoteWithKey Int |
     Asc56 NoteWithKey Int |
     Desc56 NoteWithKey Int
  deriving (Eq, Show)



-- Chord example
-- Notes ((Note C Natural 4 Quarter) (Notes (Note E Natural 4 Quarter) (Notes (Note G Natural 4 Quarter) EmptyChord)))
