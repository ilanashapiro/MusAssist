module MusAssistAST 
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
  -- btw, for Ord (no longer using): earlier constructors in the datatype declaration count as smaller than later ones

-- https://stackoverflow.com/questions/58761160/why-isnt-enum-typeclass-a-subclass-of-ord-typeclass
-- https://stackoverflow.com/questions/5684049/is-there-some-way-to-define-an-enum-in-haskell-that-wraps-around
instance Enum NoteName where
    toEnum n = case n `mod` 7 of
                  0 -> C
                  1 -> D
                  2 -> E
                  3 -> F
                  4 -> G
                  5 -> A
                  _ -> B

    fromEnum C = 0
    fromEnum D = 1
    fromEnum E = 2
    fromEnum F = 3
    fromEnum G = 4
    fromEnum A = 5
    fromEnum B = 6

    -- The generated definitions don't handle wrapping, so we need to do it ourselves
    -- source for this part: https://stackoverflow.com/questions/58761160/why-isnt-enum-typeclass-a-subclass-of-ord-typeclass
    enumFromTo n1 n2
            | n1 == n2 = [n1]
    enumFromTo n1 n2 = n1 : enumFromTo (succ n1) n2
    enumFromThenTo n1 n2 n3
            | n2 == n3 = [n1, n2]
    enumFromThenTo n1 n2 n3 = n1 : enumFromThenTo n2 (toEnum $ (2 * fromEnum n2) - (fromEnum n1)) n3

data Accidental = 
     DoubleFlat 
     | Flat 
     | Natural
     | Sharp
     | DoubleSharp
  deriving (Eq, Enum, Show, Read)

type Octave = Int -- range is [1,8]

data Inversion = 
  Root 
  | First 
  | Second 
  | Third -- seventh chords only
  deriving (Eq, Show, Read)

type Length = Int

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

data ScaleType =
    MajorScale
    | NaturalMinor
    | HarmonicMinor
    | MelodicMinor
    | Chromatic
    | Octatonic
  deriving (Eq, Show, Read)

data Direction =
    Ascending
    | Descending
  deriving (Eq, Show, Read)
  
data ChordType = 
     Triad 
     | Seventh
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

data Tone = Tone NoteName Accidental Octave 
  deriving (Eq, Show, Read)

--------------------------------------------------------------------------------
-- Musical Objects
--------------------------------------------------------------------------------

data Expr = 
  Rest Duration
  | Chord [Tone] Duration -- notes are single-element chords
  | LabeledExpr [Expr]
    deriving (Eq, Show, Read)

data Instr = 
  KeySignature Int Int -- num sharps (0-7), num flats (0-7). One of these should be zero!
  | NewMeasure 
  | Write [Expr]
  | Assign Label [Expr] -- the expanded expression is assigned to the label in IRConversion
    deriving (Eq, Show, Read)

-- templates to get expanded: these are the direct results of the parse
data IntermediateExpr = 
  Note Tone Duration -- these get expanded to become single-element chords
  | ChordTemplate Tone Quality ChordType Inversion Duration -- Predefined chords: these all happen in root position
  | Cadence CadenceType Tone Quality Duration -- quality is major/minor ONLY. det the start note and key of the cadence
  | HarmonicSequence HarmonicSequenceType Tone Quality Duration Length -- quality is major/minor ONLY. det the start note and key of the seq
  | Scale NoteName Accidental ScaleType Tone Direction Duration Length
  | Label Label -- labels referring to exprs. syntactic sugar for the expressions they contain. these get desugared before code generation
  | FinalExpr Expr
   deriving (Eq, Show, Read)

data IntermediateInstr = 
  IRKeySignature NoteName Accidental Quality
  | IRNewMeasure
  | IRWrite [IntermediateExpr]
  | IRAssign Label [IntermediateExpr] -- labeled expressions. syntactic sugar for the expressions they contain. these get desugared before code generation
    deriving (Eq, Show, Read)
-- IRWrite [Scale C Natural MajorScale (Tone E Natural 5) Ascending Quarter 10]