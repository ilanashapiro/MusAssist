{-|
Module       : MusicXMLgen
Description  : The MusicXML code emitter for the compiler -- part of the backend
Maintainer   : Ilana Shapiro
-}

module MusicXMLgen where

import qualified MusAssistAST         as MusAST
import           Data.Char
import           Data.IORef           as IORef
import qualified Data.Bimap           as Bimap
import           Control.Monad.Extra
import           Data.Map(Map)
import qualified Data.Map as Map

type CodeLine = String                    -- ^ A line of musicXML code
type NoteDuration = Int
type BeatCounter = IORef.IORef Int
type MeasureCounter = IORef.IORef Int
type KeySignature = IORef.IORef (Int, Int) -- num sharps, num flats. at least one must be 0! 

-- NOTE:
-- The user will always explicitly say whether something is sharp or flat
-- Since the user doesn't handle measures directly (except for saying "start a new measure"),
-- we don't keep track of sharps/flats per measure. MuseScore will handle that notation. 
-- MusicXML just needs to know that actual quality of the note,
-- and aside from the key signature which the user can define, we don't need to handle anything beyond this

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

globalDurationIntBimap :: Bimap.Bimap MusAST.Duration Int
globalDurationIntBimap = Bimap.fromList 
  [(MusAST.Whole, 16),
  (MusAST.DottedHalf, 12),
  (MusAST.Half, 8),
  (MusAST.DottedQuarter, 6),
  (MusAST.Quarter, 4),
  (MusAST.DottedEighth, 3),
  (MusAST.Eighth, 2),
  (MusAST.Sixteenth, 1)]

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
breakUpNoteValRationally (noteVal:noteVals) remainingTimeInMeasure isFromMeasStart = do
  if noteVal <= remainingTimeInMeasure then do
    noteDuration <- Bimap.lookupR noteVal globalDurationIntBimap 
    remainingPadding <- breakUpNoteValRationally noteVals (remainingTimeInMeasure - noteVal) isFromMeasStart
    -- breaking up note at beginning of measure, want note/rest length from longest -> shortest
    -- or, if it's end of measure padding, want note/rest  length from shortest -> longest
    return $ if isFromMeasStart then (noteDuration, noteVal):remainingPadding else remainingPadding ++ [(noteDuration, noteVal)]
  else breakUpNoteValRationally noteVals remainingTimeInMeasure isFromMeasStart

generateNoteValueRationalDivisions :: Int -> Bool -> IO [(MusAST.Duration, Int)]
-- we want the note vals in globalDurationIntBimap in desc order, biggest to smallest, in order for the greedy prop to work
generateNoteValueRationalDivisions = breakUpNoteValRationally (reverse $ Bimap.elems globalDurationIntBimap) 

generateRestsFromDivisions :: [(MusAST.Duration, Int)] -> IO [CodeLine]
generateRestsFromDivisions restDurationValPairs = return $ 
  concatMap (\(restDuration, restVal) ->
    let isMeasureRest = restVal == globalTimePerMeasure
        restTypeCode = if isMeasureRest then [] 
                       else durationToNoteTypeCode restDuration
    in ["\t\t\t<note>",
           "\t\t\t\t<rest" ++ (if isMeasureRest then " measure=\"yes\"" else "") ++ "/>",
          "\t\t\t\t<duration>" ++ show restVal ++ "</duration>",
          "\t\t\t\t<voice>1</voice>"]
        ++ restTypeCode ++
        ["\t\t\t</note>"])
  restDurationValPairs

-- all these notes will be tied BOTH WAYS
generateTiedNotesFromDivisions :: [[CodeLine]] -> [(MusAST.Duration, Int)] -> IO [CodeLine]
generateTiedNotesFromDivisions pitchesCode noteDurationValPairs = return $ 
    concatMap (\(noteDuration, noteVal) ->
      let noteTypeCode = durationToNoteTypeCode noteDuration
      in concatMap 
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
      isMeasureRest          = restDurationVal == remainingTimeInMeasure && currentBeatCount == 0
      restTypeCode           = if isMeasureRest then [] else durationToNoteTypeCode duration 

  if restDurationVal <= remainingTimeInMeasure -- rest fits in measure
    then do 
      newMeasureCode <- updateBeat restDurationVal state -- new measure code gets generated if restDurationVal == remainingTimeInMeasure
      return $ -- the code for the rest that fits in the current measure
        ["\t\t\t<note>",
        "\t\t\t\t<rest " ++ (if isMeasureRest then "measure=\"yes\"" else "") ++ "/>",
        "\t\t\t\t<duration>" ++ show restDurationVal ++ "</duration>",
        "\t\t\t\t<voice>1</voice>"]
        ++ restTypeCode ++
        ["\t\t\t</note>"]
        ++ newMeasureCode

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
                  MusAST.Sharp       -> 1
                  MusAST.DoubleSharp -> 2
                  MusAST.Flat        -> -1
                  MusAST.DoubleFlat  -> -2
                  MusAST.Natural     -> 0
                chordCode = if index > 0 then ["\t\t\t\t<chord/>"] else []
            in return $ chordCode ++
              ["\t\t\t\t<pitch>",
              "\t\t\t\t\t<step>" ++ show noteName ++ "</step>",
              "\t\t\t\t\t<alter>" ++ show alterValue ++ "</alter>", --- REPLACE THIS WITH THE ACTUAL WHEN FIXED
              "\t\t\t\t\t<octave>" ++ show octave ++ "</octave>",
              "\t\t\t\t</pitch>"]) [0..] tones)

  if noteDurationVal <= remainingTimeInMeasure -- note fits in measure
    then do 
      newMeasureCode <- updateBeat noteDurationVal state -- new measure code gets generated if noteDurationVal == remainingTimeInMeasure
      return $ 
        (concatMap -- the code for the note that fits in the current measure
          (\pitchCode ->
            ["\t\t\t<note>"]
            ++ pitchCode ++
            ["\t\t\t\t<duration>" ++ show noteDurationVal ++ "</duration>",
            "\t\t\t\t<voice>1</voice>"]
            ++ noteTypeCode ++
            ["\t\t\t</note>"]) pitchesCode)
        ++ newMeasureCode

  else do -- note does not fit in measure
    -- the code for note rest that fits in the current measure
    initialNoteDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    let (firstNoteDuration, firstNoteVal) = head initialNoteDivisions -- the first note of the tie only gets tied one way, from the right
        firstNoteTypeCode                 = durationToNoteTypeCode firstNoteDuration
        firstInitialNoteCode              = -- handle the first note of the tie sequence, which has a start tie ONLY
          concatMap 
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
    let (finalSpilledNoteDuration, finalSpilledNoteVal) = last spilledNoteDivisions -- the last note of the tie only gets tied one way, from the left
        finalSpilledNoteTypeCode                = durationToNoteTypeCode finalSpilledNoteDuration
        finalSpilledNoteCode            =  -- handle the final note of the tie sequence, which has a stop tie ONLY
          Prelude.concatMap 
            (\pitchCode -> 
              ["\t\t\t<note>"]
                ++ pitchCode ++
                ["\t\t\t\t<duration>" ++ show finalSpilledNoteVal ++ "</duration>", -- the duration of this note is all the time that's left in the measure, since the note doesn't fit in the measure
                "\t\t\t\t<tie type=\"stop\"/>", -- the note doesn't fit in the measure, so we tie
                "\t\t\t\t<voice>1</voice>"]
                ++ finalSpilledNoteTypeCode ++
                ["\t\t\t\t<notations>",
                  "\t\t\t\t\t<tied type=\"stop\"/>",
                "\t\t\t\t</notations>",
              "\t\t\t</note>"]) pitchesCode
        remainingSpilledNoteDivisions   = init spilledNoteDivisions 
    remainingSpilledNoteCode <- generateTiedNotesFromDivisions pitchesCode remainingSpilledNoteDivisions

    updateBeat remainingNoteLength state -- with current time sig/note length setup, cannot have a tied note that fills the next measure, so NO new measure code should get generated here

    return $ firstInitialNoteCode ++ remainingInitialNoteCode ++ newMeasureCode ++ remainingSpilledNoteCode ++ finalSpilledNoteCode  

transExpr state (MusAST.LabeledExpr exprs) = concatMapM (transExpr state) exprs

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
    (currentSharps, currentFlats)    <- IORef.readIORef keySig
    currentBeatCount                 <- IORef.readIORef currBeatCt
    measureNum                       <- IORef.readIORef measNum

    if numSharps == currentSharps && numFlats == currentFlats then return [] else do -- if the new key sig is same as old one, then don't do anything

    IORef.writeIORef keySig (numSharps, numFlats)
    let remainingTimeInMeasure = globalTimePerMeasure - currentBeatCount
        isStartMeasure         = currentBeatCount == 0 
        isStartFirstMeasure    = isStartMeasure && measureNum == 1
        keySigFifthsVal        = if numSharps > 0 then numSharps else -numFlats

    -- i.e. we have NOT already changed the key sig in meas 1
    -- bc if the key sig is set at beginning of piece, don't go to the next measure to do it
    -- this occurs when the first instruction in the program is a key change
    -- notably, globalHeaderCode is the rest of the header code for the file (see CompileM.hs)
    if isStartFirstMeasure && (currentSharps == 0 && currentFlats == 0) 
      then globalHeaderCode keySigFifthsVal
    else do 
    -- this includes if the user consecutively sets a bunch of key sigs at the beginning, each one will happen in a new measure
    let newKeySigCode = 
          ["\t<attributes>",
            "\t\t<key>",
              "\t\t\t<fifths>" ++ show keySigFifthsVal ++ "</fifths>",
              "\t\t</key>",
            "\t</attributes>"]
    restPaddingDivisions <- generateNoteValueRationalDivisions remainingTimeInMeasure False
    measurePadding       <- generateRestsFromDivisions restPaddingDivisions
    newMeasureCode       <- updateBeat remainingTimeInMeasure state 

    return $ if isStartMeasure then newKeySigCode else measurePadding ++ newMeasureCode ++ newKeySigCode

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

-- Assign Label [Expr]
transInstr _ _ = return [] -- we don't generate any code for assigning labels to exprs

-----------------------------------------------------------------------------------------
-- Code Generation for All the Instructions
-----------------------------------------------------------------------------------------

transInstrs :: [MusAST.Instr] -> IO [CodeLine]
transInstrs instrs = do
  beatCt        <- IORef.newIORef 0
  measureCt     <- IORef.newIORef 1
  defaultKeySig <- IORef.newIORef (0, 0) -- no sharps, no flats
  let state =  (beatCt, measureCt, defaultKeySig) 

  instrSeqs <- mapM (transInstr state) instrs
  if null instrSeqs then return [] else do
  let (currBeatCt, _, _) = state
      firstInstrSeq = head instrSeqs
      firstCodeLine = if null firstInstrSeq then [] else head firstInstrSeq -- null if first instr is Assign
      lastInstrSeq  = last instrSeqs
      lastCodeLine  = if null lastInstrSeq then [] else last lastInstrSeq -- null if last instr is Assign

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

  return $ concat (remainingHeaderCode:finalInstrs)
