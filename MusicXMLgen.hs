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
type TimeSignature = Float -- whole = 4, quarter = 1, eighth = 0.5, etc. absolute time per measure
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
updateBeat :: NoteDuration -> TimeSignature -> State -> (State, [CodeLine])
updateBeat noteDuration timePerMeasure (currBeatCt, measureCt) = do
  currentBeatCount <- Data.IORef.readIORef currBeatCt
  let updatedBeatCount = currentBeatCount + noteDuration 
  if updatedBeatCount == timePerMeasure 
    then do
      measureNum <- Data.IORef.readIORef measureCt
      let newMeasureCode = ["\t\t</measure>", "\t\t<measure number=\"" ++ measureNum ++ " width=\"165.43\">"]
      Data.IORef.writeIORef currBeatCt 0.0                    -- reset beats to 0 bc we're in a new measure
      Data.IORef.writeIORef measureCt (measureNum + 1)    -- increment the measure count
      return (state, newMeasureCode)
    else 
      Data.IORef.writeIORef currBeatCt updatedBeat
      return (state, [])

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

convertAccidentalToString :: MusAST.ACCIDENTAL -> Float


-----------------------------------------------------------------------------------------
-- Code Generation for Notes
-----------------------------------------------------------------------------------------

-- | Returns machine code that writes the given note into musicXML
transNote :: MusAST.Note -> State -> IO [CodeLine]

------------------------------------------------
-- Rests
------------------------------------------------
transNote (MusAST.REST duration) (currBeatCt, measureCt) = do
  let noteDuration = convertDurationToFloat duration
      fullMeasuresInDuration = noteDuration / 2 -- ETC, NOT DONE
  return ["<note>",
          "<rest measure=\"yes\"/>",
          "<duration>" ++ show noteDuration ++ "</duration>",
          "<voice>1</voice>",
          "</note>"]  

------------------------------------------------
-- General notes
------------------------------------------------
transNote (MusAST.Note noteName accidental octave duration) (currBeatCt, measureCt) = do
  let noteDuration = convertDurationToFloat duration
  if octave < 1 || octave > 9 then return error "Octave must be between 1 and 8"
  else return ["<note>",
            "<pitch>",
              "<step>" ++ show noteName ++ "</step>",
              "<alter>" ++ accidentalCOMPUTE ++ "</alter>", -- sharp = 1, nat = 0, flat = -1
              "<octave>" ++ show octave ++ "</octave>",
            "</pitch>",
            "<duration>" ++ noteDuration ++ "</duration>",
            "<voice>1</voice>", -- because we limit to 1 staff (treble)
          "</note>"]  


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
transPiece :: BeatCounter -> MeasureCounter -> IO [CodeLine]
xFrag ct (NT.FragCode l stmts) = do
  let initialCode = 
  instrs <- xStmts ct stmts
  return $ A.FragCode l instrs

xFrags :: Counter -> [NT.Fragment] -> IO [A.Fragment]
xFrags ct = mapM (xFrag ct)
