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
generateMeasurePadding :: Int -> [Int] -> IO [CodeLine]
generateMeasurePadding 0 _                                       = return []
generateMeasurePadding _ []                                      = return $ error "cannot generate accurate measure padding"
generateMeasurePadding remainingTimeInMeasure (noteVal:noteVals) =
  if noteVal <= remainingTimeInMeasure then do
    -- print noteVal
    -- print noteVals
    -- print ""
    noteDuration <- lookupR noteVal globalDurationIntBimap 
    let noteTypeCode = durationToNoteTypeCode noteDuration
        restCode = ["\t\t\t<note>",
                "\t\t\t\t<rest/>",
                "\t\t\t\t<duration>" ++ show noteVal ++ "</duration>",
                "\t\t\t\t<voice>1</voice>"]
              ++ noteTypeCode ++
              ["\t\t\t</note>"] 
    remainingPadding <- generateMeasurePadding (remainingTimeInMeasure - noteVal) noteVals 
    return $ remainingPadding ++ restCode -- rest order is shortest to longest in the measusre
  else do
    print noteVal
    print noteVals
    print remainingTimeInMeasure
    generateMeasurePadding remainingTimeInMeasure noteVals

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
  let (currBeatCt, measureCt, keySignature) = state
  measureNum <- IORef.readIORef measureCt
  currentBeatCount <- IORef.readIORef currBeatCt

  noteDurationVal <- Bimap.lookup duration globalDurationIntBimap 
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
      noteTypeCode = if noteDurationVal == remainingTimeInMeasure then [] 
                      else durationToNoteTypeCode duration -- for rests ONLY

  if noteDurationVal <= remainingTimeInMeasure -- case 1: note fits in measure
    then do 
      updateBeat noteDurationVal state -- update the beat, but there's no new measure code
      return $ -- the code for the note that fits in the current measure
        ["\t\t\t<note>",
        "\t\t\t\t<rest " ++ (if noteDurationVal == remainingTimeInMeasure then "measure=\"yes\"" else "") ++ "/>",
        "\t\t\t\t<duration>" ++ show noteDurationVal ++ "</duration>",
        "\t\t\t\t<voice>1</voice>"]
        ++ noteTypeCode ++
        ["\t\t\t</note>"]

  else do
    let initialNoteCode = -- the code for the note that fits in the current measure
          ["\t\t\t<note>",
            "\t\t\t\t<rest/>",
            "\t\t\t\t<duration>" ++ show remainingTimeInMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            -- "\t\t\t\t<tie type=\"start\"/>", -- the note doesn't fit in the measure, so we tie
            "\t\t\t\t<voice>1</voice>"
            -- "\t\t\t\t<notations>",
            --   "\t\t\t\t\t<tie type=\"start\"/>",
            -- "\t\t\t\t</notations>",
          ] ++ noteTypeCode ++
          ["\t\t\t</note>"] 
    newMeasureCode <- updateBeat remainingTimeInMeasure state
    let remainingNoteLength = noteDurationVal - remainingTimeInMeasure
        tiedNoteCode =
          ["\t\t\t<note>",
            "\t\t\t\t<rest/>",
            "\t\t\t\t<duration>" ++ show remainingNoteLength ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            -- "\t\t\t\t<tie type=\"stop\"/>", -- last note of ties has end tie only
            "\t\t\t\t<voice>1</voice>",
            -- "\t\t\t\t<notations>",
            --   "\t\t\t\t\t<tie type=\"stop\"/>",
            -- "\t\t\t\t</notations>",
          "\t\t\t</note>"] 

    return $ initialNoteCode ++ newMeasureCode ++ tiedNoteCode
            
------------------------------------------------
-- Anything that remains untranslated prints a warning message
-- When you think you're done, this should probably be replaced
-- by a call to error.
------------------------------------------------
transExpr _ _ = do
  putStrLn "  unknown MusAST expr "
  return []

-- cases :
-- 1) note fits in the current measure = DONE 
-- 2) note doesn't fit in current measure, but fits in the next measure 
-- 3) note doesn't fit in current measure, but fits precisely in an exact number of additional measures
-- 4) note doesn't fit in current measure, spills over 1+ additional measure, and then partially spills over a final measure

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
-- Code Generation for Fragments
-----------------------------------------------------------------------------------------

-- | Turn list of MusAssist AST abstract syntax into musicXML code
transInstr :: State -> MusAST.Instr -> IO [CodeLine]
transInstr state (MusAST.KeySignature numSharps numFlats) = undefined
transInstr state (MusAST.Assign label expr) = undefined
transInstr state (MusAST.Write exprs)
  | exprs == [] = return[]
  | otherwise = do 
      let (currBeatCt, _, _) = state
      code <- concatMapM (transExpr state) exprs
      finalBeatCount <- IORef.readIORef currBeatCt
      finalMeasFill <- if finalBeatCount > 0 -- i.e. we're in the middle of a measure
                then let remainingTimeInMeasure = globalTimePerMeasure - finalBeatCount
                    in generateMeasurePadding remainingTimeInMeasure (reverse $ elems globalDurationIntBimap) -- we want the note vals in desc order, biggest to smallest
              else return []
      return $ code ++ finalMeasFill 

transInstr (currBeatCt, _, _) MusAST.NewMeasure = 
  currentBeatCount = IORef.readIORef currBeatCt
  if currentBeatCount > 0 -- i.e. we're in the middle of a measure
    then let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
             measurePadding = generateMeasurePadding remainingTimeInMeasure (reverse $ elems globalDurationIntBimap)
        in generateMeasurePadding remainingTimeInMeasure (reverse $ elems globalDurationIntBimap) -- we want the note vals in desc order, biggest to smallest
  else updateBeat

transInstrs :: State -> [MusAST.Instr] -> IO [CodeLine]
transInstrs state instrs = do
  instrSeqs <- mapM (transInstr state) instrs
  return $ Prelude.concat instrSeqs
