module MusAssistAST 
-- (
--   Chord (..) -- this is the export list, the ... is the constructors of that type
-- if I don't put anything here, than everything is exported. it's actually a means of data hiding.
-- )
where
--------------------------------------------------------------------------------
-- Sub-pieces of musical objects
--------------------------------------------------------------------------------

-- might have to create map if I need to index
data NoteName = 
    F
    | C
    | G 
    | D 
    | E 
    | A 
    | B
  deriving (Eq, Ord, Show, Read) -- the notes are ordered in the order of sharps. reverse for order of flats
  -- earlier constructors in the datatype declaration count as smaller than later ones

data Accidental = 
     Natural 
     | Sharp 
     | Flat
  deriving (Eq, Show, Read)

type Octave = Int -- range is [1,8]

type Inversion = Int -- range is [1,4]

type Length = Int -- range is [1,4]

type Label = String -- for saving sequences of notes/chords

data Duration = 
    Sixteenth
    | Eighth 
    | DottedEighth 
    | Quarter 
    | DottedQuarter 
    | DottedHalf 
    | Half 
    | Whole 
  deriving (Eq, Show, Ord, Read)

data Quality = 
     Major 
     | Minor
     | Augmented      -- chords only
     | Diminished     -- chords only
     | HalfDiminished -- seventh chords only
  deriving (Eq, Show, Read)

data ChordType = 
     Triad 
     | Seventh
  deriving (Eq, Show, Read)
--------------------------------------------------------------------------------
-- Musical Objects
--------------------------------------------------------------------------------
data Tone = Tone NoteName Accidental Octave 
  deriving (Eq, Show, Read)
    
-- all resulting chords in root position
data CadenceType = 
  PerfAuth 
  | ImperfAuth
  | Plagal
  | HalfCad
  | Deceptive
  deriving (Eq, Show, Read)

-- all resulting chords in root position
data HarmonicSequenceType = 
  AscFifths
  | DescFifths
  | Asc56 
  | Desc56
  deriving (Eq, Show, Read)

data Expr = 
  Rest Duration
  -- can keep this and predefined chords, bc if I just had custom chord, it's harder to work with
  -- with DSLs, keep the domain specific information for as long as possible for expanding the generation
  -- if I didn't, all I had is custom chord, then I give the user the ability to use the nice template, but
  -- I also took away the ability for the tool to take advantage of the semantic info the user is giving
  -- granted, I could def recover it by reconstructing custom chord, but if the user is already giving this, 
  -- then why recover it. we want to take advantage of the props of the DSL!
  -- analogy: in a GPL, keep the loop as long as possible before converting to JUMP
  | Chord [Tone] Duration -- notes are single-element chords
    deriving (Eq, Show, Read)

data Instr = 
  KeySignature Int Int -- num sharps (0-7), num flats (0-7). One of these should be zero!
  | NewMeasure 
  | Write [Expr]
    deriving (Eq, Show, Read)

-- templates to get expanded: these are the direct results of the parse
 -- plan: translate from one intermediate representation to another. in my case, I can maybe do this intermediate
    -- translation in which I lower these things (Chord, Cadence, HarmSeq) into their simplified form (i.e. CustomChords)
    -- and then the code generation is just for NOTES, rests ,and custom chords
data IntermediateExpr = 
  Note Tone Duration -- these get expanded to become single-element chords
  | ChordTemplate Tone Quality ChordType Inversion Duration -- Predefined chords: these all happen in root position
  | Cadence CadenceType Tone Quality -- quality is major/minor ONLY. det the start note and key of the cadence
  | HarmonicSequence HarmonicSequenceType Tone Quality Duration Length -- quality is major/minor ONLY. det the start note and key of the seq
  | FinalExpr Expr
   deriving (Eq, Show, Read)

data IntermediateInstr = 
  SetKeySignature NoteName Quality
  | CreateNewMeasure
  | IRWrite [IntermediateExpr]
    deriving (Eq, Show, Read)
