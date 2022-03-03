{-|
Module       : X86gen
Description  : The x86 code emitter for the compiler -- part of the backend
Maintainer   : CS 132
-}

module MusicXMLgen where

import qualified MusAssistAST    as MusAST
-- import Control.Monad 
import           Data.IORef
import           Data.Char

type CodeLine = String                    -- ^ A line of musicXML code
type BeatCounter = Data.IORef.IORef Float
type MeasureCounter = Data.IORef.IORef Int
type NoteDuration = Float 
type CumulativeDuration = Float 
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

timePerMeasure :: Float
timePerMeasure = 4 -- whole = 4, quarter = 1, eighth = 0.5, etc. absolute time per measure is set at 4, from common time
---------------------------------------------
-- Beats: to handle measures --
---------------------------------------------

-- | Update where we're at in a measure and handle new measures
updateBeat :: NoteDuration -> State -> IO [CodeLine]
updateBeat noteDuration (currBeatCt, measureCt, _) = do
  currentBeatCount <- Data.IORef.readIORef currBeatCt
  let updatedBeatCount = currentBeatCount + noteDuration 
  if updatedBeatCount == timePerMeasure 
    then do
      measureNum <- Data.IORef.readIORef measureCt
      Data.IORef.writeIORef currBeatCt 0.0                    -- reset beats to 0 bc we're in a new measure
      let incMeasNum = measureNum + 1
      Data.IORef.writeIORef measureCt incMeasNum    -- increment the measure count
      let newMeasureCode = ["\t\t</measure>", "\t\t<measure number=\"" ++ show incMeasNum ++ "\">"]
      return newMeasureCode
    else do
      Data.IORef.writeIORef currBeatCt updatedBeatCount
      return []

convertDurationToFloat :: MusAST.Duration -> Float
convertDurationToFloat duration =
  case duration of
    MusAST.Whole         -> 4
    MusAST.DottedHalf    -> 3
    MusAST.Half          -> 2
    MusAST.DottedQuarter -> 1.5
    MusAST.Quarter       -> 1
    MusAST.DottedEighth  -> 0.75
    MusAST.Eighth        -> 0.5
    MusAST.Sixteenth     -> 0.25

convertDurationToNoteType :: MusAST.Duration -> [CodeLine]
convertDurationToNoteType duration = case duration of
        MusAST.DottedHalf    -> ["\t\t\t\t<type>half</type>", "<dot/>"]
        MusAST.DottedQuarter -> ["\t\t\t\t<type>quarter</type>", "<dot/>"]
        MusAST.DottedEighth  -> ["\t\t\t\t<type>eighth</type>", "<dot/>"]
        MusAST.Sixteenth     -> ["\t\t\t\t<type>16th</type>"]
        _                    -> ["\t\t\t\t<type>" ++ (toLower (head durationStr):tail durationStr) ++ "</type>"]
                                  where durationStr = show duration

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
  measureNum <- Data.IORef.readIORef measureCt
  currentBeatCount <- Data.IORef.readIORef currBeatCt

  let noteDuration = convertDurationToFloat duration
      remainingTimeInMeasure = timePerMeasure - currentBeatCount
      noteTypeCode = convertDurationToNoteType duration

  if noteDuration <= remainingTimeInMeasure -- case 1: note fits in measure
    then do 
      updateBeat noteDuration state -- update the beat, but there's no new measure code
      return $
        [ -- the code for the note that fits in the current measure
        "\t\t\t<note>",
        "\t\t\t\t<rest/>",
        "\t\t\t\t<duration>" ++ show noteDuration ++ "</duration>",
        "\t\t\t\t<voice>1</voice>"
        -- "<type>" ++ toLower (show duration) ++ "</type>",
        ] ++ noteTypeCode ++
        ["\t\t\t</note>"]

  else do
    let fullMeasuresInDuration = floor (noteDuration / timePerMeasure)
        initialNoteCode = -- the code for the note that fits in the current measure
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

    let generateTiedMeasures remainingNoteLength 
          | remainingNoteLength <= timePerMeasure = return
            ["\t\t\t<note>",
              "\t\t\t\t<rest/>",
              "\t\t\t\t<duration>" ++ show remainingNoteLength ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
              -- "\t\t\t\t<tie type=\"stop\"/>", -- last note of ties has end tie only
              "\t\t\t\t<voice>1</voice>",
              -- "\t\t\t\t<notations>",
              --   "\t\t\t\t\t<tie type=\"stop\"/>",
              -- "\t\t\t\t</notations>",
            "\t\t\t</note>"] 
            -- no more tied measures, and so remainingNoteLength fits in this measure
          | otherwise = do
              newMeasureCode <- updateBeat remainingTimeInMeasure state
              tiedMeasures <- generateTiedMeasures (remainingNoteLength - timePerMeasure)
              let noteCode = 
                    ["\t\t\t<note>",
                      "\t\t\t\t<rest measure=\"yes\"/>",
                      "\t\t\t\t<duration>" ++ show timePerMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
                      -- "\t\t\t\t<tie type=\"start\"/>", -- note in middle of tie has both start and stop ties
                      -- "\t\t\t\t<tie type=\"stop\"/>", 
                      "\t\t\t\t<voice>1</voice>",
                      -- "\t\t\t\t<notations>",
                      --   "\t\t\t\t\t<tie type=\"start\"/>",
                      --   "\t\t\t\t\t<tie type=\"stop\"/>",
                      -- "\t\t\t\t</notations>",
                    "\t\t\t</note>"] 
              return $ newMeasureCode ++ noteCode ++ tiedMeasures
    tiedMeasuresCode <- generateTiedMeasures (noteDuration - remainingTimeInMeasure)
    return $ initialNoteCode ++ newMeasureCode ++ tiedMeasuresCode
            
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
transInstr state (MusAST.Write expr) = do 
  let (currBeatCt, _, _) = state
  code <- transExpr state expr
  finalBeatCount <- Data.IORef.readIORef currBeatCt
  print finalBeatCount
  let finalMeasFill = 
          if finalBeatCount < timePerMeasure && finalBeatCount > 0 -- i.e. we're in the middle of a measure
            then let remainingTimeInMeasure = timePerMeasure - finalBeatCount
                    --  noteTypeCode = convertDurationToNoteType duration
              in ["\t\t\t<note>",
                  "\t\t\t\t<rest/>",
                  "\t\t\t\t<duration>" ++ show remainingTimeInMeasure ++ "</duration>",
                  "\t\t\t\t<voice>1</voice>",
                -- ++ noteTypeCode ++
                "\t\t\t</note>"] 
          else []
  return $ code ++ finalMeasFill 
  
  
transInstr state MusAST.NewMeasure = undefined

transInstrs :: State -> [MusAST.Instr] -> IO [CodeLine]
transInstrs state instrs = do
  instrSeqs <- mapM (transInstr state) instrs
  return $ Prelude.concat instrSeqs
