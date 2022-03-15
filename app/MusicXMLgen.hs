{-|
Module       : MusicXMLgen
Description  : The MusicXML code emitter for the compiler -- part of the backend
Maintainer   : Ilana Shapiro
-}

module MusicXMLgen where

import qualified MusAssistAST         as MusAST
import           Data.IORef           as IORef
import           Data.Bimap           as Bimap
import           Control.Monad.Extra
import           Data.Map(Map)
import qualified Data.Map as Map

type CodeLine = String                    -- ^ A line of musicXML code
type NoteDuration = Int
type BeatCounter = IORef.IORef Int
type MeasureCounter = IORef.IORef Int
type KeySignature = IORef.IORef (Int, Int) -- num sharps, num flats. at least one must be 0! 

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

globalHeaderCode :: Int -> IO [CodeLine]
globalHeaderCode fifths = return $ 
  ["\t<attributes>",
  "\t\t<divisions>4</divisions>", -- 4 divisions because the smallest note is 1/16
  "\t\t<key>",
    "\t\t\t<fifths>" ++ show fifths ++ "</fifths>",
    "\t\t</key>",
  "\t\t<time>",
    "\t\t\t<beats>4</beats>",
    "\t\t\t<beat-type>4</beat-type>",
    "\t\t</time>",
  "\t\t<clef>",
    "\t\t\t<sign>G</sign>",
    "\t\t\t<line>2</line>",
    "\t\t</clef>",
  "\t</attributes>"] 

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
    let isMeasureRest = restVal == globalTimePerMeasure
        restTypeCode = if isMeasureRest then [] 
                       else durationToNoteTypeCode restDuration
    in ["\t\t\t<note>",
           "\t\t\t\t<rest " ++ (if isMeasureRest then "measure=\"yes\"" else "") ++ "/>",
          "\t\t\t\t<duration>" ++ show restVal ++ "</duration>",
          "\t\t\t\t<voice>1</voice>"]
        ++ restTypeCode ++
        ["\t\t\t</note>"])
  restDurationValPairs

-- all these notes will be tied BOTH WAYS
generateTiedNotesFromDivisions :: [[CodeLine]] -> [(MusAST.Duration, Int)] -> IO [CodeLine]
generateTiedNotesFromDivisions pitchesCode noteDurationValPairs = return $ 
    Prelude.concatMap (\(noteDuration, noteVal) ->
      let noteTypeCode = durationToNoteTypeCode noteDuration
      in Prelude.concatMap 
          (\pitchCode -> 
            ["\t\t\t<note>"]
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
            "\t\t\t</note>"]) pitchesCode)
          noteDurationValPairs

-----------------------------------------------------------------------------------------
-- Code Generation for Musical Expressions
-----------------------------------------------------------------------------------------
transExpr :: State -> MusAST.Expr -> IO [CodeLine]

------------------------------------------------
-- Rests
------------------------------------------------
transExpr state (MusAST.Rest duration) = do
  let (currBeatCt, measureCt, _) = state
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

------------------------------------------------------------------------------------------------
-- Chords (a Note is a single-element Chord)
------------------------------------------------------------------------------------------------
transExpr state (MusAST.Chord tones duration) = do 
  let (currBeatCt, measureCt, _) = state
  measureNum                      <- IORef.readIORef measureCt
  currentBeatCount                <- IORef.readIORef currBeatCt
  noteDurationVal                 <- Bimap.lookup duration globalDurationIntBimap 
  let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
      noteTypeCode = durationToNoteTypeCode duration 
      pitchesCode = concat (zipWith
        (\index (MusAST.Tone noteName accidental octave) ->
          if octave < 1 || octave > 8 then return $ error "octave must be between 1 and 8 inclusive" else
            let alterValue = case accidental of
                  MusAST.Sharp -> 1
                  MusAST.Flat -> -1
                  MusAST.Natural -> 0
                chordCode = if index > 0 then ["\t\t\t\t<chord/>"] else []
            in return $ 
            chordCode ++
            ["\t\t\t\t<pitch>",
            "\t\t\t\t\t<step>" ++ show noteName ++ "</step>",
            "\t\t\t\t\t<alter>" ++ show alterValue ++ "</alter>", --- REPLACE THIS WITH THE ACTUAL WHEN FIXED
            "\t\t\t\t\t<octave>" ++ show octave ++ "</octave>",
            "\t\t\t\t</pitch>"]) [0..] tones)

  if noteDurationVal <= remainingTimeInMeasure -- note fits in measure
    then do 
      updateBeat noteDurationVal state -- update the beat, but there's no new measure code
      return $ -- the code for the note that fits in the current measure
        Prelude.concatMap 
          (\pitchCode ->
            ["\t\t\t<note>"]
            ++ pitchCode ++
            ["\t\t\t\t<duration>" ++ show noteDurationVal ++ "</duration>",
            "\t\t\t\t<voice>1</voice>"]
            ++ noteTypeCode ++
            ["\t\t\t</note>"]) pitchesCode

  else do -- note does not fit in measure
    -- the code for note rest that fits in the current measure
    initialNoteDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    let (firstNoteDuration, firstNoteVal) = head initialNoteDivisions -- the first note of the tie only gets tied one way, from the right
        firstNoteTypeCode                 = durationToNoteTypeCode firstNoteDuration
        firstInitialNoteCode              = -- handle the first note of the tie sequence, which has a start tie ONLY
          Prelude.concatMap 
          (\pitchCode -> 
            ["\t\t\t<note>"]
              ++ pitchCode ++
              ["\t\t\t\t<duration>" ++ show firstNoteVal ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
              "\t\t\t\t<tie type=\"start\"/>", -- the note doesn't fit in the measure, so we tie
              "\t\t\t\t<voice>1</voice>"]
              ++ firstNoteTypeCode ++
              ["\t\t\t\t<notations>",
                "\t\t\t\t\t<tied type=\"start\"/>",
              "\t\t\t\t</notations>",
            "\t\t\t</note>"]) pitchesCode
        remainingInitialNoteDivisions = tail initialNoteDivisions
    remainingInitialNoteCode <- generateTiedNotesFromDivisions pitchesCode remainingInitialNoteDivisions -- generating the remaining tied notes that fit in this measure (each note is tied both ways)
    
    newMeasureCode <- updateBeat remainingTimeInMeasure state

    -- the code for the rest that spills into the next measure
    let remainingNoteLength = noteDurationVal - remainingTimeInMeasure

    -- generating the remaining tied notes that fit in this measure (each note is tied both ways)
    -- True bc start of measure, so we want rests from greatest -> least
    spilledNoteDivisions <- generateNoteValueRationalDivisions remainingNoteLength True 
    let (lastNoteDuration, lastNoteVal) = last spilledNoteDivisions -- the last note of the tie only gets tied one way, from the left
        lastNoteTypeCode                = durationToNoteTypeCode lastNoteDuration
        finalSpilledNoteCode            =  -- handle the final note of the tie sequence, which has a stop tie ONLY
          Prelude.concatMap 
            (\pitchCode -> 
              ["\t\t\t<note>"]
                ++ pitchCode ++
                ["\t\t\t\t<duration>" ++ show lastNoteVal ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
                "\t\t\t\t<tie type=\"stop\"/>", -- the note doesn't fit in the measure, so we tie
                "\t\t\t\t<voice>1</voice>"]
                ++ lastNoteTypeCode ++
                ["\t\t\t\t<notations>",
                  "\t\t\t\t\t<tied type=\"stop\"/>",
                "\t\t\t\t</notations>",
              "\t\t\t</note>"]) pitchesCode
        remainingSpilledNoteDivisions   = tail spilledNoteDivisions 
    remainingSpilledNoteCode <- generateTiedNotesFromDivisions pitchesCode remainingSpilledNoteDivisions

    updateBeat remainingNoteLength state -- with current time sig/note length setup, cannot have a tied note that fills the next measure, so NO new measure code should get generated here

    return $ firstInitialNoteCode ++ remainingInitialNoteCode ++ newMeasureCode ++ remainingSpilledNoteCode ++ finalSpilledNoteCode   

-----------------------------------------------------------------------------------------
-- Code Generation for Individual Instructions
-----------------------------------------------------------------------------------------

-- | Turn list of MusAssist AST abstract syntax into musicXML code
transInstr :: State -> MusAST.Instr -> IO [CodeLine]

transInstr state (MusAST.KeySignature numSharps numFlats) =
  if numSharps < 0 || numFlats < 0 
    || numSharps > 7 || numFlats > 7
    || numSharps > 0 && numFlats > 0
  then return $ error "key sig must have 0-7 sharps OR flats, not both!" 
  else do   
    let (currBeatCt, measNum, keySig) = state 
    currentBeatCount <- IORef.readIORef currBeatCt
    measureNum <- IORef.readIORef measNum
    IORef.writeIORef keySig (numSharps, numFlats)
    let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
        isStartFirstMeasure    = currentBeatCount == 0 && measureNum == 1
        keySigFifthsVal = if numSharps > 0 then numSharps else -numFlats

    -- if the key sig is set at beginning of piece, don't go to the next measure to do it
    -- in fact, this is the rest of the header code for the file (see CompileM.hs)
    if isStartFirstMeasure then globalHeaderCode keySigFifthsVal else do 
    
    let newKeySigCode = 
          ["\t<attributes>",
            "\t\t<key>",
              "\t\t\t<fifths>" ++ show keySigFifthsVal ++ "</fifths>",
              "\t\t</key>",
            "\t</attributes>"]
    restPaddingDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    measurePadding       <- generateRestsFromDivisions restPaddingDivisions
    newMeasureCode       <- updateBeat remainingTimeInMeasure state

    return $ measurePadding ++ newMeasureCode ++ newKeySigCode

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

-----------------------------------------------------------------------------------------
-- Code Generation for All the Instructions
-----------------------------------------------------------------------------------------

transInstrs :: State -> [MusAST.Instr] -> IO [CodeLine]
transInstrs state instrs = do
  instrSeqs <- mapM (transInstr state) instrs
  let (currBeatCt, _, _) = state
      firstInstrSeq = head instrSeqs
      firstCodeLine = head firstInstrSeq
      lastInstrSeq  = last instrSeqs
      lastCodeLine  = last lastInstrSeq
  
  -- this is the remaining header code for the MusicXML file that was started in CompileM.hs
  -- we set it here to have key sig of no sharps and flats, if the user's first instruction 
  -- was not to set a custom key sig for the start of the piece
  remainingHeaderCode <- if firstCodeLine /= "\t<attributes>" then globalHeaderCode 0 else return []

  finalBeatCount <- IORef.readIORef currBeatCt
  finalInstrs <-
    if finalBeatCount > 0 || lastCodeLine == "\t</attributes>" -- we're in the middle of a measure, or we just did a key change
      then do 
        let remainingTimeInMeasure = globalTimePerMeasure - finalBeatCount
        restPaddingDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
        finalMeasureFill <- generateRestsFromDivisions restPaddingDivisions
        return $ instrSeqs ++ [finalMeasureFill]
      else do
        let finalizedCode = init instrSeqs
        return $ finalizedCode ++ [take ((length lastInstrSeq) - 2) lastInstrSeq] -- remove the hanging new measure code since we do not want it

  return $ Prelude.concat finalInstrs
