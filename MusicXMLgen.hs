{-|
Module       : X86gen
Description  : The x86 code emitter for the compiler -- part of the backend
Maintainer   : CS 132
-}

module ToMusicXML where

import qualified MusAssistAST    as MusAST
--import Control.Monad 

type CodeLine = String                    -- ^ A line of musicXML code
type BeatCounter = Data.IORef.IORef Float
type MeasureCounter = Data.IORef.IORef Int32
type NoteDuration = Float 
type CumulativeDuration = Float 
type TimePerMeasure = Float -- whole = 4, quarter = 1, eighth = 0.5, etc. absolute time per measure
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

type State = (BeatCounter, MeasureCounter)





---------------------------------------------
-- Beats: to handle measures --
---------------------------------------------

-- | Update where we're at in a measure and handle new measures
updateBeat :: NoteDuration -> TimePerMeasure -> State -> [CodeLine]
updateBeat noteDuration timePerMeasure (currBeatCt, measureCt) = do
  currentBeatCount <- Data.IORef.readIORef currBeatCt
  let updatedBeatCount = currentBeatCount + noteDuration 
  if updatedBeatCount == timePerMeasure 
    then do
      measureNum <- Data.IORef.readIORef measureCt
      let newMeasureCode = ["\t\t</measure>", "\t\t<measure number=\"" ++ measureNum ++ ">"]
      Data.IORef.writeIORef currBeatCt 0.0                    -- reset beats to 0 bc we're in a new measure
      Data.IORef.writeIORef measureCt (measureNum + 1)    -- increment the measure count
      return newMeasureCode
    else 
      Data.IORef.writeIORef currBeatCt updatedBeat
      return []

convertDurationToFloat :: MusAST.DURATION -> Float
convertDurationToFloat duration =
  case duration of
    Whole         -> 4
    DottedHalf    -> 3
    Half          -> 2
    DottedQuarter -> 1.5
    Quarter       -> 1
    DottedEighth  -> 0.75
    Eighth        -> 0.5
    Sixteenth     -> 0.25

-- convertAccidentalToString :: MusAST.ACCIDENTAL -> Float

-- generateTiedMeasures :: Int -> [CodeLine]
-- generateTiedMeasures 1  = 
--                   [ "<measure number=\"" ++ show measureNum ++ "\" width=\"232.96\">",
--                     "<note>",
--                       "<rest measure=\"yes\"/>",
--                       "<duration>" ++ show duration ++ "</duration>",
--                       "<tie type=\"stop\"/>",
--                       "<tie type=\"start\"/>",
--                       "<voice>1</voice>",
--                       "<type>whole</type>",
--                       "<notations>",
--                         "<tied type=\"stop\"/>",
--                         "<tied type=\"start\"/>",
--                         "</notations>",
--                       "</note>",
--                     "</measure>"]
--                generateTiedMeasures ms = 

-----------------------------------------------------------------------------------------
-- Code Generation for Notes
-----------------------------------------------------------------------------------------

-- | Returns machine code that writes the given note into musicXML
transNote :: MusAST.Note -> TimePerMeasure -> State -> IO [CodeLine]

------------------------------------------------
-- Rests
------------------------------------------------
transNote (MusAST.REST duration) timePerMeasure (currBeatCt, measureCt) = do
  measureNum <- Data.IORef.readIORef measureCt
  currentBeatCount <- Data.IORef.readIORef currBeatCt

  let noteDuration = convertDurationToFloat duration
      remainingTimeInMeasure = timePerMeasure - currentBeatCount

  if noteDuration <= remainingTimeInMeasure -- case 1: note fits in measure
    then do 
      updateBeat noteDuration timePerMeasure (currBeatCt, measureCt) -- update the beat, but there's no new measure code
      return [ -- the code for the note that fits in the current measure
        "\t\t\t<note>",
        "\t\t\t\t<rest/>",
        "\t\t\t\t<duration>" ++ show noteDuration ++ "</duration>",
        "\t\t\t\t<voice>1</voice>",
        -- "<type>" ++ toLower (show duration) ++ "</type>",
        "\t\t\t</note>"] 

  else 
    let fullMeasuresInDuration = floor (noteDuration / timePerMeasure)
        initialNoteCode = -- the code for the note that fits in the current measure
          ["\t\t\t<note>",
            "\t\t\t\t<rest/>",
            "\t\t\t\t<duration>" ++ show remainingTimeInMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            -- "\t\t\t\t<tie type=\"start\"/>", -- the note doesn't fit in the measure, so we tie
            "\t\t\t\t<voice>1</voice>",
            -- "\t\t\t\t<notations>",
            --   "\t\t\t\t\t<tie type=\"start\"/>",
            -- "\t\t\t\t</notations>",
          "\t\t\t</note>"] 

        generateTiedMeasures remainingNoteLength 
          | remainingNoteLength <= timePerMeasure = newMeasureCode ++ noteCode ++ generateTiedMeasures (remainingNoteLength - timePerMeasure)
            ["\t\t\t<note>",
              "\t\t\t\t<rest/>",
              "\t\t\t\t<duration>" ++ show remainingNoteLength ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
              "\t\t\t\t<tie type=\"stop\"/>", -- last note of ties has end tie only
              "\t\t\t\t<voice>1</voice>",
              -- "\t\t\t\t<notations>",
              --   "\t\t\t\t\t<tie type=\"stop\"/>",
              -- "\t\t\t\t</notations>",
            "\t\t\t</note>"] 
            -- no more tied measures, and so remainingNoteLength fits in this measure
          | otherwise = newMeasureCode ++ noteCode ++ generateTiedMeasures (remainingNoteLength - timePerMeasure)
            where newMeasureCode = updateBeat remainingTimeInMeasure timePerMeasure (currBeatCt, measureCt)
                  noteCode = 
                    ["\t\t\t<note>",
                      "\t\t\t\t<rest measure=\"yes\"/>",,
                      "\t\t\t\t<duration>" ++ show timePerMeasure ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
                      -- "\t\t\t\t<tie type=\"start\"/>", -- note in middle of tie has both start and stop ties
                      -- "\t\t\t\t<tie type=\"stop\"/>", 
                      "\t\t\t\t<voice>1</voice>",
                      -- "\t\t\t\t<notations>",
                      --   "\t\t\t\t\t<tie type=\"start\"/>",
                      --   "\t\t\t\t\t<tie type=\"stop\"/>",
                      -- "\t\t\t\t</notations>",
                    "\t\t\t</note>"] 
            

-- cases :
-- 1) note fits in the current measure = DONE 
-- 2) note doesn't fit in current measure, but fits in the next measure 
-- 3) note doesn't fit in current measure, but fits precisely in an exact number of additional measures
-- 4) note doesn't fit in current measure, spills over 1+ additional measure, and then partially spills over a final measure

  -- continuingTieCode = if currentBeatCount == 0 then ["<tie type=\"stop\"/>"] else []
  -- if fullMeasuresInDuration > 0
  --     then let generateTiedMeasures 1  = 
  --                 [ "<measure number=\"" ++ show measureNum ++ "\" width=\"232.96\">",
  --                   "<note>",
  --                     "<rest measure=\"yes\"/>",
  --                     "<duration>" ++ show duration ++ "</duration>",
  --                     "<tie type=\"stop\"/>",
  --                     "<tie type=\"start\"/>",
  --                     "<voice>1</voice>",
  --                     "<type>whole</type>",
  --                     "<notations>",
  --                       "<tied type=\"stop\"/>",
  --                       "<tied type=\"start\"/>",
  --                       "</notations>",
  --                     "</note>",
  --                   "</measure>"]
  --              generateTiedMeasures ms = 
  --                ["<note>",
  --                 "<rest measure=\"yes\"/>",
  --                 "<duration>" ++ show noteDuration ++ "</duration>",
  --                 "<voice>1</voice>",
  --                 "</note>"]
  --                 ++ generateTiedMeasures (ms - 1)

  -- return ["<note>",
  --         "<rest measure=\"yes\"/>",
  --         "<duration>" ++ show noteDuration ++ "</duration>",
  --         "<voice>1</voice>",
  --          "<type>" ++ toLower (show duration) ++ "</type>",
  --         "</note>"]  

------------------------------------------------
-- General notes
------------------------------------------------
transNote (MusAST.Note noteName accidental octave duration) (currBeatCt, measureCt) = do
  let noteDuration = convertDurationToFloat duration
  if octave < 1 || octave > 9 then return error "Octave must be between 1 and 8"
  else return [] 
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
transChord :: MusAST.Chord -> State -> IO [CodeLine]

transChord MusAST.EmptyChord _ = return []

transChord (MusAST.ChordNotes note customChord) (beatCt, measureCt) = undefined

transChord (MusAST.Chord note quality chordType inversion) (beatCt, measureCt) = undefined

------------------------------------------------
-- Anything that remains untranslated prints a warning message
-- When you think you're done, this should probably be replaced
-- by a call to error.
------------------------------------------------
xStmt _ stmt = do
  putStrLn ("  unknown xStmt " ++ show stmt)
  return []

-----------------------------------------------------------------------------------------
-- Code Generation for Fragments
-----------------------------------------------------------------------------------------

-- | Turns an NT Fragment into the equivalent A.Fragment by just doing
--   instruction-selection on the code parts
transPiece :: BeatCounter -> MeasureCounter -> MusAST.Instr -> IO [CodeLine]
xFrag measureCt = do
  notes <- transNote 
  transNote :: MusAST.Note -> TimePerMeasure -> State -> IO [CodeLine]
  return $ A.FragCode l instrs

xFrags :: Counter -> [NT.Fragment] -> IO [A.Fragment]
xFrags ct = mapM (xFrag ct)
