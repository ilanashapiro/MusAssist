{-|
Module       : X86gen
Description  : The x86 code emitter for the compiler -- part of the backend
Maintainer   : CS 132
-}

module MusicXMLgen where

import qualified MusAssistAST         as MusAST
import           Data.IORef           as IORef
import           Data.Char
import           Data.Bimap           as Bimap
import           Control.Monad.Extra

type CodeLine = String                    -- ^ A line of musicXML code
type BeatCounter = IORef.IORef Int
type MeasureCounter = IORef.IORef Int
type NoteDuration = Int
type KeySignature = (Maybe MusAST.NoteName, Maybe MusAST.NoteName) -- last sharp in key sig, last flat in key sig (one should always be Nothing!)

-- IN CONCLUSION
-- The user will always explicitly say whether something is sharp or flat
-- Since the user doesn't handle measures directly (except for saying "start a new measure"),
-- don't keep track of sharps/flats per measure, since MuseScore will handle that notation. 
-- MusicXML just wants to know that actual quality of the note,
-- and aside from key signature which the user can define, we don't need to handle anything beyond this
------------------------------------------------------------------------------------------------------------------------------------------------
----type NotePrimitive = (MusAST.NoteName, MusAST.Octave) -- just saves note name and octave, use for keeping track of sharp/flat notes in a measure
-- list of sharp notes (i.e. notename, octave), list of flat notes in current measure
-- resets to empty at start of each measure
----type SharpsFlatsAccInMeas = ([NotePrimitive], [NotePrimitive])

type State = (BeatCounter, MeasureCounter, KeySignature)

globalTimePerMeasure :: Int
globalTimePerMeasure = 16 -- whole = 16, quarter = 4, eighth = 2, etc. absolute time per measure is set at 16, from common time

globalOrderOfSharps :: [MusAST.NoteName]
globalTimePerMeasure = [F, C, G, D, A, E, B]
---------------------------------------------
-- Beats: to handle measures --
---------------------------------------------

-- | Update where we're at in a measure and handle new measures
updateBeat :: NoteDuration -> State -> IO [CodeLine]
updateBeat noteDuration (currBeatCt, measureCt, _) = do
  currentBeatCount <- IORef.readIORef currBeatCt
  let updatedBeatCount = currentBeatCount + noteDuration 
  if updatedBeatCount == globalTimePerMeasure 
    then do
      measureNum <- IORef.readIORef measureCt
      IORef.writeIORef currBeatCt 0                    -- reset beats to 0 bc we're in a new measure
      let incMeasNum = measureNum + 1
      IORef.writeIORef measureCt incMeasNum    -- increment the measure count
      let newMeasureCode = ["\t\t</measure>", "\t\t<measure number=\"" ++ show incMeasNum ++ "\">"]
      return newMeasureCode
    else do
      IORef.writeIORef currBeatCt updatedBeatCount
      return []

globalDurationIntBimap :: Bimap MusAST.Duration Int
globalDurationIntBimap = Bimap.fromList 
  [(MusAST.Sixteenth, 1),
  (MusAST.Eighth, 2),
  (MusAST.DottedEighth, 3),
  (MusAST.Quarter, 4),
  (MusAST.DottedQuarter, 6),
  (MusAST.Half, 8),
  (MusAST.DottedHalf, 12),
  (MusAST.Whole, 16)]

durationToNoteTypeCode :: MusAST.Duration -> [CodeLine]
durationToNoteTypeCode duration = case duration of
  MusAST.DottedHalf    -> ["\t\t\t\t<type>half</type>", "<dot/>"]
  MusAST.DottedQuarter -> ["\t\t\t\t<type>quarter</type>", "<dot/>"]
  MusAST.DottedEighth  -> ["\t\t\t\t<type>eighth</type>", "<dot/>"]
  MusAST.Sixteenth     -> ["\t\t\t\t<type>16th</type>"]
  _                    -> ["\t\t\t\t<type>" ++ (toLower (head durationStr):tail durationStr) ++ "</type>"]
                            where durationStr = show duration

-- go through all the note vals possible (there's only 8), and generate rest types greedily via the largest notes that fit in the measure
-- note: does NOT update the beat
generateMeasurePadding :: Int -> [Int] -> IO [CodeLine]
generateMeasurePadding 0 _                                       = return []
generateMeasurePadding _ []                                      = return $ error "cannot generate accurate measure padding"
generateMeasurePadding remainingTimeInMeasure (noteVal:noteVals) =
  if noteVal <= remainingTimeInMeasure then do
    noteDuration <- lookupR noteVal globalDurationIntBimap 
    let noteTypeCode = if noteVal == remainingTimeInMeasure then [] 
                      else durationToNoteTypeCode noteDuration -- for rests ONLY
        restCode = ["\t\t\t<note>",
                "\t\t\t\t<rest/>",
                "\t\t\t\t<duration>" ++ show noteVal ++ "</duration>",
                "\t\t\t\t<voice>1</voice>"]
              ++ noteTypeCode ++
              ["\t\t\t</note>"] 
    remainingPadding <- generateMeasurePadding (remainingTimeInMeasure - noteVal) noteVals 
    return $ remainingPadding ++ restCode -- rest order is shortest to longest in the measusre
  else generateMeasurePadding remainingTimeInMeasure noteVals

-- convertAccidentalToString :: MusAST.ACCIDENTAL -> Float

-----------------------------------------------------------------------------------------
-- Code Generation for Notes
-----------------------------------------------------------------------------------------

-- | Returns machine code that writes the given note into musicXML
transExpr :: State -> MusAST.Expr -> IO [CodeLine]

------------------------------------------------
-- Rests
------------------------------------------------
transExpr state (MusAST.Rest duration) = do
  let (currBeatCt, measureCt, keySig) = state
  measureNum            <- IORef.readIORef measureCt
  currentBeatCount      <- IORef.readIORef currBeatCt

  noteDurationVal <- Bimap.lookup duration globalDurationIntBimap 
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
      noteTypeCode = if noteDurationVal == remainingTimeInMeasure then [] 
                      else durationToNoteTypeCode duration -- for rests ONLY

  if noteDurationVal <= remainingTimeInMeasure -- note fits in measure
    then do 
      updateBeat noteDurationVal state -- update the beat, but there's no new measure code
      return $ -- the code for the note that fits in the current measure
        ["\t\t\t<note>",
        "\t\t\t\t<rest " ++ (if noteDurationVal == remainingTimeInMeasure then "measure=\"yes\"" else "") ++ "/>",
        "\t\t\t\t<duration>" ++ show noteDurationVal ++ "</duration>",
        "\t\t\t\t<voice>1</voice>"]
        ++ noteTypeCode ++
        ["\t\t\t</note>"]

  else do -- note does not fit in measure
    let initialNoteCode = -- the code for the note that fits in the current measure
          ["\t\t\t<note>",
            "\t\t\t\t<rest/>",
            "\t\t\t\t<duration>" ++ show remainingTimeInMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            "\t\t\t\t<voice>1</voice>"
          ] ++ noteTypeCode ++
          ["\t\t\t</note>"] 
    newMeasureCode <- updateBeat remainingTimeInMeasure state
    let remainingNoteLength = noteDurationVal - remainingTimeInMeasure
        tiedNoteCode =
          ["\t\t\t<note>",
            "\t\t\t\t<rest/>",
            "\t\t\t\t<duration>" ++ show remainingNoteLength ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            "\t\t\t\t<voice>1</voice>",
          "\t\t\t</note>"] 
    updateBeat remainingNoteLength state -- note: with current time sig/note length setup, cannot have a tied note that fills the next measure, so no new measure code should get generated here

    return $ initialNoteCode ++ newMeasureCode ++ tiedNoteCode

------------------------------------------------
-- Sounding Notes
------------------------------------------------
 transExpr state (MusAST.Note (MusAST.Tone noteName accidental octave) duration) = do
  if octave < 1 || octave > 8 then return $ error "octave must be between 1 and 8 inclusive" else
  let (currBeatCt, measureCt, keySignature) = state
  measureNum            <- IORef.readIORef measureCt
  currentBeatCount      <- IORef.readIORef currBeatCt
  (numSharps, numFlats) <- IORef.readIORef keySig
  noteDurationVal       <- Bimap.lookup duration globalDurationIntBimap 
  
  let alterValue =
    if numSharps > 0 then 
      lastSharpInKeySig = globalOrderOfSharps !! numSharps
      noteName > 
  -- [1,2,3]!!1


  let noteTypeCode = durationToNoteTypeCode duration 
      pitchCode = 
        ["\t\t\t\t<pitch>",
        "\t\t\t\t\t<step>" ++ show noteName ++ "</step>",
        "\t\t\t\t\t<alter>" ++ 1 ++ "</alter>"
        "\t\t\t\t\t<octave>" ++ show octave ++ "</octave>",
        "\t\t\t\t</pitch>"]

  if noteDurationVal <= remainingTimeInMeasure -- note fits in measure
    then do 
      updateBeat noteDurationVal state -- update the beat, but there's no new measure code
      return $ -- the code for the note that fits in the current measure
        ["\t\t\t<note>"]
        ++ pitchCode ++
        ["\t\t\t\t<duration>" ++ show noteDurationVal ++ "</duration>",
        "\t\t\t\t<voice>1</voice>"]
        ++ noteTypeCode ++
        ["\t\t\t</note>"]

  else do -- note does not fit in measure
    let initialNoteCode = -- the code for the note that fits in the current measure
          ["\t\t\t<note>"]
            ++ pitchCode ++
            ["\t\t\t\t<duration>" ++ show remainingTimeInMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            "\t\t\t\t<tie type=\"start\"/>", -- the note doesn't fit in the measure, so we tie
            "\t\t\t\t<voice>1</voice>"
            "\t\t\t\t<notations>",
              "\t\t\t\t\t<tie type=\"start\"/>",
            "\t\t\t\t</notations>",
          ] ++ noteTypeCode ++
          ["\t\t\t</note>"] 
    newMeasureCode <- updateBeat remainingTimeInMeasure state
    let remainingNoteLength = noteDurationVal - remainingTimeInMeasure
        tiedNoteCode =
          ["\t\t\t<note>"]
           ++ pitchCode ++
            ["\t\t\t\t<duration>" ++ show remainingNoteLength ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            "\t\t\t\t<tie type=\"stop\"/>", -- last note of ties has end tie only
            "\t\t\t\t<voice>1</voice>",
            "\t\t\t\t<notations>",
              "\t\t\t\t\t<tie type=\"stop\"/>",
            "\t\t\t\t</notations>",
          "\t\t\t</note>"] 
    updateBeat remainingNoteLength state -- note: with current time sig/note length setup, cannot have a tied note that fills the next measure, so no new measure code should get generated here

    return $ initialNoteCode ++ newMeasureCode ++ tiedNoteCode           
------------------------------------------------
-- Anything that remains untranslated prints a warning message
-- When you think you're done, this should probably be replaced
-- by a call to error.
------------------------------------------------
transExpr _ _ = do
  putStrLn "  unknown MusAST expr "
  return []

------------------------------------------------
-- General notes
------------------------------------------------
-- transNote (MusAST.Note noteName accidental octave duration) (currBeatCt, measureCt) = do
--   let noteDuration = convertDurationToFloat duration
--   if octave < 1 || octave > 9 then return error "Octave must be between 1 and 8"
--   else return [] 
  -- ["<note>",
  --           "<pitch>",
  --             "<step>" ++ show noteName ++ "</step>",
  --             "<alter>" ++ accidentalCOMPUTE ++ "</alter>", -- sharp = 1, nat = 0, flat = -1
  --             "<octave>" ++ show octave ++ "</octave>",
  --           "</pitch>",
  --           "<duration>" ++ noteDuration ++ "</duration>",
  --           "<voice>1</voice>", -- because we limit to 1 staff (treble)
  --         "</note>"]  


-----------------------------------------------------------------------------------------
-- Code Generation for Chords
-----------------------------------------------------------------------------------------

-- | Returns machine code that writes the given chord (i.e. a certain collection of notes) into musicXML
-- transChord :: MusAST.Chord -> State -> IO [CodeLine]

-- transChord MusAST.EmptyChord _ = return []

-- transChord (MusAST.ChordNotes note customChord) (beatCt, measureCt, timePerMeasure, keySignature) = undefined

-- transChord (MusAST.Chord note quality chordType inversion) (beatCt, measureCt, timePerMeasure, keySignature) = undefined



-----------------------------------------------------------------------------------------
-- Code Generation for Instructions
-----------------------------------------------------------------------------------------

-- | Turn list of MusAssist AST abstract syntax into musicXML code
transInstr :: State -> MusAST.Instr -> IO [CodeLine]

-- type KeySignature = (Maybe MusAST.NoteName, Maybe MusAST.NoteName) -- last sharp in key sig, last flat in key sig (one should always be Nothing!)
transInstr state (MusAST.KeySignature numSharps numFlats) = 
  if numSharps < 0 || numFlats < 0 
    || numSharps > 7 || numFlats > 7
    || numSharps > 0 && numFlats > 0
  return $ error "key sig must have 0-7 sharps OR flats, not both!" else

  let (currBeatCt, _, keySig) = state
  if numSharps > 0 ----- NEED TO CONVERT TO LAST SHARP AND FLAT NAMES, FROM THE NUMBERS
  IORef.writeIORef keySig (lastSharp, lastFlat)
  currentBeatCount <- IORef.readIORef currBeatCt
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
  measurePadding <- generateMeasurePadding remainingTimeInMeasure (reverse $ elems globalDurationIntBimap)  -- we want the note vals in desc order, biggest to smallest
  newMeasureCode <- updateBeat remainingTimeInMeasure state

  let keySigFifthsVal = if numSharps > 0 then numSharps else -numFlats
    newKeySigCode = 
    ["\t<attributes>",
        "\t\t<key>",
          "\t\t\t<fifths>" ++ show keySigFifthsVal ++ "</fifths>",
          "\t\t</key>",
        "\t</attributes>"]

  return $ measurePadding ++ newMeasureCode ++ newKeySigCode

  
transInstr state (MusAST.Assign label expr) = undefined

transInstr state (MusAST.Write exprs)
  | exprs == [] = return []
  | otherwise = concatMapM (transExpr state) exprs

transInstr state MusAST.NewMeasure = do
  let (currBeatCt, _, _) = state
  currentBeatCount <- IORef.readIORef currBeatCt
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
  measurePadding <- generateMeasurePadding remainingTimeInMeasure (reverse $ elems globalDurationIntBimap)  -- we want the note vals in desc order, biggest to smallest
  newMeasureCode <- updateBeat remainingTimeInMeasure state
  return $ measurePadding ++ newMeasureCode

transInstrs :: State -> [MusAST.Instr] -> IO [CodeLine]
transInstrs state instrs = do
  instrSeqs <- mapM (transInstr state) instrs
  let (currBeatCt, _, _) = state
  finalBeatCount <- IORef.readIORef currBeatCt
  finalInstrs <-
    if finalBeatCount > 0 -- i.e. we're in the middle of a measure
      then do 
        let remainingTimeInMeasure = globalTimePerMeasure - finalBeatCount
        finalMeasureFill <- generateMeasurePadding remainingTimeInMeasure (reverse $ elems globalDurationIntBimap) -- we want the note vals in desc order, biggest to smallest
        return $ instrSeqs ++ [finalMeasureFill]
      else do
        let finalizedCode = init instrSeqs
            lastInstrSeq = last instrSeqs
        return $ finalizedCode ++ [take ((length lastInstrSeq) - 2) lastInstrSeq] -- remove the hanging new measure code since we do not want it
  return $ Prelude.concat finalInstrs
