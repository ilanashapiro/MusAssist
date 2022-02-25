module MusAssistAST 
(
  Chord (..)
)
where
--------------------------------------------------------------------------------
-- Data that is never used alone (they are used to create the tokens down below)
--------------------------------------------------------------------------------

data NoteName = 
     C 
     | D 
     | E 
     | F 
     | G 
     | A 
     | B
  deriving (Eq, Show)

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

--------------------------------------------------------------------------------
-- Tokens that get translated from user input
--------------------------------------------------------------------------------
data Note = 
     Note NoteName Accidental Octave Duration 
     | Rest
  deriving (Eq, Show)

-- | Custom collection of notes
data CustomChord = 
     EmptyChord 
     | Notes Note CustomChord
  deriving (Eq, Show)

-- | Predefined chords: these all happen in root position
data Chord = Chord Note Quality ChordType Inversion -- note cannot be a rest
  deriving (Eq, Show)

data Cadence = 
     PerfAuth NoteName Quality 
     | ImperfAuth NoteName Quality 
     | Plagal NoteName Quality 
     | HalfCad NoteName Quality 
     | Deceptive NoteName Quality 
  deriving (Eq, Show)


-- Chord example
-- Notes ((Note C Natural 4 Quarter) (Notes (Note E Natural 4 Quarter) (Notes (Note G Natural 4 Quarter) EmptyChord)))
