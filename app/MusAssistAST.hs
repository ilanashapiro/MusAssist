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
  deriving (Eq, Enum, Bounded, Show, Read)

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
  | Label Label
    deriving (Eq, Show, Read)

data Instr = 
  KeySignature Int Int -- num sharps (0-7), num flats (0-7). One of these should be zero!
  | NewMeasure 
  | Write [Expr]
  | Assign Label [Expr] -- the expanded expression is assigned to the label
    deriving (Eq, Show, Read)

-- templates to get expanded: these are the direct results of the parse
 -- plan: translate from one intermediate representation to another. in my case, I can maybe do this intermediate
    -- translation in which I lower these things (Chord, Cadence, HarmSeq) into their simplified form (i.e. CustomChords)
    -- and then the code generation is just for NOTES, rests ,and custom chords
data IntermediateExpr = 
  Note Tone Duration -- these get expanded to become single-element chords
  | ChordTemplate Tone Quality ChordType Inversion Duration -- Predefined chords: these all happen in root position
  | Cadence CadenceType Tone Quality Duration -- quality is major/minor ONLY. det the start note and key of the cadence
  | HarmonicSequence HarmonicSequenceType Tone Quality Duration Length -- quality is major/minor ONLY. det the start note and key of the seq
  | FinalExpr Expr
   deriving (Eq, Show, Read)

data IntermediateInstr = 
  IRKeySignature NoteName Accidental Quality
  | IRNewMeasure
  | IRWrite [IntermediateExpr]
  | IRAssign Label [IntermediateExpr] -- labeled expressions. syntactic sugar for the expressions they contain. these get desugared before code generation
    deriving (Eq, Show, Read)
