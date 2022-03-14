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
import           Data.Map(Map)
import qualified Data.Map as Map

type CodeLine = String                    -- ^ A line of musicXML code
type NoteDuration = Int
type BeatCounter = IORef.IORef Int
type MeasureCounter = IORef.IORef Int
type KeySignature = IORef.IORef (Maybe MusAST.NoteName, Maybe MusAST.NoteName) -- last sharp in key sig, last flat in key sig (at least one should always be Nothing!)
-- type NoteAlterMap = IORef.IORef (Map MusAST.NoteName Int) -- maps note names to alter pitch vals: -1, 0, or 1. default is 0

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

type State = (BeatCounter, MeasureCounter, KeySignature)--, NoteAlterMap)

globalTimePerMeasure :: Int
globalTimePerMeasure = 16 -- whole = 16, quarter = 4, eighth = 2, etc. absolute time per measure is set at 16, from common time

globalOrderOfSharps :: [MusAST.NoteName]
globalOrderOfSharps = [MusAST.F, MusAST.C, MusAST.G, MusAST.D, MusAST.A, MusAST.E, MusAST.B]

-- globalDefaultNoteAlterMap :: Map MusAST.NoteName Int
-- globalDefaultNoteAlterMap = Map.fromList
--   [(MusAST.F, 0), 
--   (MusAST.C, 0), 
--   (MusAST.G, 0), 
--   (MusAST.D, 0), 
--   (MusAST.A, 0),
--   (MusAST.E, 0),
--   (MusAST.B, 0)]
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
      measureNum           <- IORef.readIORef measureCt
      IORef.writeIORef currBeatCt 0                    -- reset beats to 0 bc we're in a new measure
      let incMeasNum = measureNum + 1
      IORef.writeIORef measureCt incMeasNum    -- increment the measure count
      -- IORef.writeIORef noteAlterMap globalDefaultNoteAlterMap -- reset all accidentals per measure to default (i.e. none)
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
  MusAST.DottedHalf    -> ["\t\t\t\t<type>half</type>", "\t\t\t\t<dot/>"]
  MusAST.DottedQuarter -> ["\t\t\t\t<type>quarter</type>", "\t\t\t\t<dot/>"]
  MusAST.DottedEighth  -> ["\t\t\t\t<type>eighth</type>", "\t\t\t\t<dot/>"]
  MusAST.Sixteenth     -> ["\t\t\t\t<type>16th</type>"]
  _                    -> ["\t\t\t\t<type>" ++ (toLower (head durationStr):tail durationStr) ++ "</type>"]
                            where durationStr = show duration

-- go through all the note vals possible (there's only 8), and generate note durations greedily via the largest notes that fit in the measure
-- note: does NOT update the beat, assumes that the note DOES fit in the measure
breakUpNoteValRationally :: [Int] -> Int -> Bool -> IO [(MusAST.Duration, Int)]
breakUpNoteValRationally _ 0 _                                      = return []
breakUpNoteValRationally [] _ _                                     = return $ error "cannot generate accurate note divisions" 
breakUpNoteValRationally (noteVal:noteVals) remainingTimeInMeasure isFromMeasStart =
  if noteVal <= remainingTimeInMeasure then do
    noteDuration <- lookupR noteVal globalDurationIntBimap 
    -- let noteTypeCode = durationToNoteTypeCode noteDuration
    --     restCode = ["\t\t\t<note>",
    --             "\t\t\t\t<rest/>",
    --             "\t\t\t\t<duration>" ++ show noteVal ++ "</duration>",
    --             "\t\t\t\t<voice>1</voice>"]
    --           ++ noteTypeCode ++
    --           ["\t\t\t</note>"] 
    remainingPadding <- breakUpNoteValRationally noteVals (remainingTimeInMeasure - noteVal) isFromMeasStart
    -- breaking up note at beginning of measure, want note/rest length from longest -> shortest
    -- or, if it's end of measure padding, want note/rest  length from shortest -> longest
    return $ if isFromMeasStart then (noteDuration, noteVal):remainingPadding else remainingPadding ++ [(noteDuration, noteVal)]
  else breakUpNoteValRationally noteVals remainingTimeInMeasure isFromMeasStart

generateNoteValueRationalDivisions :: Int -> Bool -> IO [(MusAST.Duration, Int)]
generateNoteValueRationalDivisions = breakUpNoteValRationally (reverse $ elems globalDurationIntBimap) -- we want the note vals in desc order, biggest to smallest, in order for the greedy prop to work

generateRestsFromDivisions :: [(MusAST.Duration, Int)] -> IO [CodeLine]
generateRestsFromDivisions restDurationValPairs = return $ 
  Prelude.concatMap (\(restDuration, restVal) ->
    let restTypeCode = durationToNoteTypeCode restDuration
    in ["\t\t\t<note>",
          "\t\t\t\t<rest/>",
          "\t\t\t\t<duration>" ++ show restVal ++ "</duration>",
          "\t\t\t\t<voice>1</voice>"]
        ++ restTypeCode ++
        ["\t\t\t</note>"])
  restDurationValPairs

-- all these notes will be tied BOTH WAYS
generateTiedNotesFromDivisions :: [CodeLine] -> [(MusAST.Duration, Int)] -> IO [CodeLine]
generateTiedNotesFromDivisions pitchCode noteDurationValPairs = return $ 
    Prelude.concatMap (\(noteDuration, noteVal) ->
      let noteTypeCode = durationToNoteTypeCode noteDuration
      in ["\t\t\t<note>"]
              ++ pitchCode ++
              ["\t\t\t\t<duration>" ++ show noteVal ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
              "\t\t\t\t<tie type=\"start\"/>",
              "\t\t\t\t<tie type=\"stop\"/>",
              "\t\t\t\t<voice>1</voice>"]
              ++ noteTypeCode ++
              ["\t\t\t\t<notations>",
                "\t\t\t\t\t<tied type=\"start\"/>",
                "\t\t\t\t\t<tied type=\"stop\"/>",
              "\t\t\t\t</notations>",
            "\t\t\t</note>"])
    noteDurationValPairs

-----------------------------------------------------------------------------------------
-- Code Generation for Musical Expressions
-----------------------------------------------------------------------------------------
transExpr :: State -> MusAST.Expr -> IO [CodeLine]

------------------------------------------------
-- Rests
------------------------------------------------
transExpr state (MusAST.Rest duration) = do
  let (currBeatCt, measureCt, keySig) = state
  measureNum            <- IORef.readIORef measureCt
  currentBeatCount      <- IORef.readIORef currBeatCt

  restDurationVal <- Bimap.lookup duration globalDurationIntBimap 
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
      restTypeCode = if restDurationVal == remainingTimeInMeasure then [] 
                      else durationToNoteTypeCode duration -- for rests ONLY

  if restDurationVal <= remainingTimeInMeasure -- rest fits in measure
    then do 
      updateBeat restDurationVal state -- update the beat, but there's no new measure code
      return $ -- the code for the rest that fits in the current measure
        ["\t\t\t<note>",
        "\t\t\t\t<rest " ++ (if restDurationVal == remainingTimeInMeasure then "measure=\"yes\"" else "") ++ "/>",
        "\t\t\t\t<duration>" ++ show restDurationVal ++ "</duration>",
        "\t\t\t\t<voice>1</voice>"]
        ++ restTypeCode ++
        ["\t\t\t</note>"]

  else do -- rest does not fit in measure
    -- the code for the rest that fits in the current measure
    initialRestDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    initialRestCode <- generateRestsFromDivisions initialRestDivisions
        
    newMeasureCode <- updateBeat remainingTimeInMeasure state
    
    -- the code for the rest that spills into the next measure
    let remainingRestLength = restDurationVal - remainingTimeInMeasure
    spilledRestDivisions <- generateNoteValueRationalDivisions remainingRestLength True
    spilledRestCode <- generateRestsFromDivisions spilledRestDivisions

    updateBeat remainingRestLength state -- with current time sig/note length setup, cannot have a tied note that fills the next measure, so NO new measure code should get generated here

    return $ initialRestCode ++ newMeasureCode ++ spilledRestCode

------------------------------------------------
-- Sounding Notes
------------------------------------------------
transExpr state (MusAST.Note (MusAST.Tone noteName accidental octave) duration) =
  if octave < 1 || octave > 8 then return $ error "octave must be between 1 and 8 inclusive" else do
  let (currBeatCt, measureCt, keySig) = state
  measureNum                      <- IORef.readIORef measureCt
  currentBeatCount                <- IORef.readIORef currBeatCt
  (lastSharpMaybe, lastFlatMaybe) <- IORef.readIORef keySig
  noteDurationVal                 <- Bimap.lookup duration globalDurationIntBimap 
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount

  let alterValue = case accidental of
                    MusAST.Sharp -> 1
                    MusAST.Flat -> -1
                    MusAST.Natural -> 0

  let noteTypeCode = durationToNoteTypeCode duration 
      pitchCode = 
        ["\t\t\t\t<pitch>",
        "\t\t\t\t\t<step>" ++ show noteName ++ "</step>",
        "\t\t\t\t\t<alter>" ++ show alterValue ++ "</alter>", --- REPLACE THIS WITH THE ACTUAL WHEN FIXED
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
    -- the code for note rest that fits in the current measure
    initialNoteDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    let (firstNoteDuration, firstNoteVal) = head initialNoteDivisions -- the first note of the tie only gets tied one way, from the right
        firstNoteTypeCode                 = durationToNoteTypeCode firstNoteDuration
        firstInitialNoteCode              = -- handle the first note of the tie sequence, which has a start tie ONLY
          ["\t\t\t<note>"]
            ++ pitchCode ++
            ["\t\t\t\t<duration>" ++ show firstNoteVal ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            "\t\t\t\t<tie type=\"start\"/>", -- the note doesn't fit in the measure, so we tie
            "\t\t\t\t<voice>1</voice>"]
            ++ firstNoteTypeCode ++
            ["\t\t\t\t<notations>",
              "\t\t\t\t\t<tied type=\"start\"/>",
            "\t\t\t\t</notations>",
          "\t\t\t</note>"]
        remainingInitialNoteDivisions = tail initialNoteDivisions
    remainingInitialNoteCode <- generateTiedNotesFromDivisions pitchCode remainingInitialNoteDivisions -- generating the remaining tied notes that fit in this measure (each note is tied both ways)
    
    newMeasureCode <- updateBeat remainingTimeInMeasure state

    -- the code for the rest that spills into the next measure
    let remainingNoteLength = noteDurationVal - remainingTimeInMeasure

    -- generating the remaining tied notes that fit in this measure (each note is tied both ways)
    -- True bc start of measure, so we want rests from greatest -> least
    spilledNoteDivisions <- generateNoteValueRationalDivisions remainingNoteLength True 
    let (lastNoteDuration, lastNoteVal) = last spilledNoteDivisions -- the last note of the tie only gets tied one way, from the left
        lastNoteTypeCode              = durationToNoteTypeCode lastNoteDuration
        finalSpilledNoteCode           =  -- handle the final note of the tie sequence, which has a stop tie ONLY
          ["\t\t\t<note>"]
            ++ pitchCode ++
            ["\t\t\t\t<duration>" ++ show lastNoteVal ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
            "\t\t\t\t<tie type=\"stop\"/>", -- the note doesn't fit in the measure, so we tie
            "\t\t\t\t<voice>1</voice>"]
            ++ lastNoteTypeCode ++
            ["\t\t\t\t<notations>",
              "\t\t\t\t\t<tied type=\"stop\"/>",
            "\t\t\t\t</notations>",
          "\t\t\t</note>"]
        remainingSpilledNoteDivisions = tail spilledNoteDivisions 
    remainingSpilledNoteCode <- generateTiedNotesFromDivisions pitchCode remainingSpilledNoteDivisions

    updateBeat remainingNoteLength state -- with current time sig/note length setup, cannot have a tied note that fills the next measure, so NO new measure code should get generated here

    return $ firstInitialNoteCode ++ remainingInitialNoteCode ++ newMeasureCode ++ remainingSpilledNoteCode ++ finalSpilledNoteCode    

------------------------------------------------
-- Anything that remains untranslated prints a warning message
-- When you think you're done, this should probably be replaced
-- by a call to error.
------------------------------------------------
transExpr _ _ = do
  putStrLn "  unknown MusAST expr "
  return []

-----------------------------------------------------------------------------------------
-- Code Generation for Instructions
-----------------------------------------------------------------------------------------

-- | Turn list of MusAssist AST abstract syntax into musicXML code
transInstr :: State -> MusAST.Instr -> IO [CodeLine]

-- type KeySignature = (Maybe MusAST.NoteName, Maybe MusAST.NoteName) -- last sharp in key sig, last flat in key sig (one should always be Nothing!)
transInstr state (MusAST.KeySignature numSharps numFlats) = 
  -- if numSharps < 0 || numFlats < 0 
  --   || numSharps > 7 || numFlats > 7
  --   || numSharps > 0 && numFlats > 0
  -- then return $ error "key sig must have 0-7 sharps OR flats, not both!" 
  -- else 
    do
    let (currBeatCt, _, keySig) = state
        lastSharp = 
          if numSharps > 0 then Just (globalOrderOfSharps !! (numSharps - 1)) -- zero indexing
          else Nothing
        lastFlat = 
          if numFlats > 0 then Just (globalOrderOfSharps !! (length globalOrderOfSharps - numSharps))
          else Nothing
    IORef.writeIORef keySig (lastSharp, lastFlat)

    currentBeatCount <- IORef.readIORef currBeatCt
    let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
    restPaddingDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    measurePadding <- generateRestsFromDivisions restPaddingDivisions
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
  restPaddingDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
  measurePadding <- generateRestsFromDivisions restPaddingDivisions
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
        restPaddingDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
        finalMeasureFill <- generateRestsFromDivisions restPaddingDivisions
        return $ instrSeqs ++ [finalMeasureFill]
      else do
        let finalizedCode = init instrSeqs
            lastInstrSeq = last instrSeqs
        return $ finalizedCode ++ [take ((length lastInstrSeq) - 2) lastInstrSeq] -- remove the hanging new measure code since we do not want it
  return $ Prelude.concat finalInstrs
