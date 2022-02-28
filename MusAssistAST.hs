module MusAssistAST 
(
  Chord (..)
)
where
--------------------------------------------------------------------------------
-- Sub-pieces of musical objects
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

type Length = Int -- range is [1,4]

type Label = String -- for saving sequences of notes/chords

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

data NoteWithKey = Key Note Quality -- quality is major/minor ONLY
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Musical Objects
--------------------------------------------------------------------------------
data Note = 
  Note NoteName Accidental Octave 
  | Rest
  deriving (Eq, Show)

-- | Predefined chords: these all happen in root position
data Chord = 
  PredefinedChord Note Quality ChordType Inversion -- note cannot be a rest
  | CustomChord [Note]
  deriving (Eq, Show)

data MusState = 
  TimeSignature Int Duration -- number of beats, beat value 
  | KeySignature NoteName Quality
  | NewMeasure

-- all resulting chords in root position
data CadenceType = 
     PerfAuth 
     | ImperfAuth
     | Plagal
     | HalfCad
     | Deceptive
  deriving (Eq, Show)

-- all resulting chords in root position
data HarmonicSequenceType = 
  AscFifths
  | DescFifths
  | Asc56 
  | Desc56
  deriving (Eq, Show)

data Expr = 
  Note Note Duration
  | Chord Chord Duration
  | Cadence CadenceType Note Quality -- quality is major/minor ONLY. det the start note and key of the cadence
  | HarmonicSequence HarmonicSequenceType Note Quality Length -- quality is major/minor ONLY. det the start note and key of the seq
  
data Instr = 
  Set MusState
  | ASSIGN Label Expr -- save a chunk of music to a label
  | WRITE Expr
