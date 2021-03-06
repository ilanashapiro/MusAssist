{-|
Module       : IRConversion
Description  : The intermediate representation for the compiler -- part of the middle. Expands chord/cadence/harmseq templates, etc
Maintainer   : Ilana Shapiro
-}

module IRConversion where

import qualified MusAssistAST         as MusAST
import           Data.List
import           Data.Either
import           Control.Monad.Extra
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.IORef         as IORef

type SymbolTable = IORef.IORef (Map MusAST.Label [MusAST.Expr]) -- for storing labeled exprs. both of these are string aliases

globalValidKeyQualities :: [MusAST.Quality]
globalValidKeyQualities = [MusAST.Major, MusAST.Minor]

globalOrderOfSharps :: [MusAST.NoteName]
globalOrderOfSharps = [MusAST.F, MusAST.C, MusAST.G, MusAST.D, MusAST.A, MusAST.E, MusAST.B]

applyN :: (a -> a) -> a -> Int -> a
applyN f x 0 = x
applyN f x n = f (applyN f x (n-1)) 

convertInversionToInt :: MusAST.Inversion -> Int 
convertInversionToInt inversion = 
    case inversion of
        MusAST.Root   -> 0
        MusAST.First  -> 1
        MusAST.Second -> 2
        MusAST.Third  -> 3

-- https://stackoverflow.com/questions/30729326/perform-a-function-on-first-elements-of-list
incrementFirstNElems :: Int -> [Int] -> [Int]
incrementFirstNElems n l = map succ left ++ right
                           where (left,right) = splitAt n l

-- Map numStepsFromTonic ([notes that are special cases for this interval], 
                        -- function to alter accidental for special case notes, 
                        -- quality that the computed accidentals are valid for).
globalStepsFromTonicToAccInfoMap :: Map Int ([MusAST.NoteName], MusAST.Accidental -> MusAST.Accidental, Maybe MusAST.Quality)
globalStepsFromTonicToAccInfoMap = Map.fromList
    [(0, ([], (succ . pred), Nothing)),                            -- root
     (1, (drop 5 globalOrderOfSharps, succ, Nothing)),             -- seconds
     (2, (take 3 globalOrderOfSharps, pred, Just MusAST.Minor)),   -- thirds
     (3, ([MusAST.F], pred, Nothing)),                             -- fourths
     (4, ([MusAST.B], succ, Nothing)),                             -- fifths
     (5, (drop 4 globalOrderOfSharps, succ, Just MusAST.Major)),   -- sixths
     (6, (take 2 globalOrderOfSharps, pred, Just MusAST.Minor))]   -- sevenths

generateToneWithinScale :: MusAST.Tone -> MusAST.Quality -> Int -> [MusAST.NoteName] -> (MusAST.Octave -> MusAST.Octave) -> IO MusAST.Tone
generateToneWithinScale tonicTone tonicQuality intervalVal specialOctaveCases octFunc = 
    if intervalVal < 0 || intervalVal > 6 then return $ error "Can't generate tone outside single scale range" else 
    if tonicQuality `notElem` globalValidKeyQualities then return $ error "Can't generate tone given invalid key quality (not major or minor)" else do
    let (MusAST.Tone tonicNoteName tonicAccidental tonicOctave) = tonicTone
        noteName   = applyN succ tonicNoteName intervalVal
        (specialAccidentalCases, accFunc, accFuncValidQuality) = 
            case (Map.lookup intervalVal globalStepsFromTonicToAccInfoMap) of 
                Just accInfo -> accInfo 
                Nothing      -> error "Invalid inverval for tone within scale generation"
        computedAcc = if tonicNoteName `elem` specialAccidentalCases then accFunc tonicAccidental else tonicAccidental
        accAdjustedForKey = case accFuncValidQuality of 
            Nothing             -> computedAcc
            Just MusAST.Major   -> 
                if tonicQuality == MusAST.Major then computedAcc 
                 else pred computedAcc -- i.e if valid quality is major, and we want minor, go down half step
            Just MusAST.Minor   -> 
                if tonicQuality == MusAST.Minor then computedAcc 
                else succ computedAcc -- i.e if valid quality is minor, and we want major, go up half step
        octave     = if tonicNoteName `elem` specialOctaveCases then octFunc tonicOctave else tonicOctave
    return $ MusAST.Tone noteName accAdjustedForKey octave

generateTriadWithinScale :: SymbolTable -> MusAST.Tone -> MusAST.Quality -> MusAST.Duration -> Int -> [MusAST.NoteName] -> (MusAST.Octave -> MusAST.Octave) -> MusAST.Inversion -> IO MusAST.Expr
generateTriadWithinScale symbolTable tonicTone tonicQuality duration intervalVal specialOctaveCases octFunc inversion = do
    let quality = case tonicQuality of 
            MusAST.Major -> 
                if intervalVal == 6 then MusAST.Diminished
                else if intervalVal `elem` [1,2,5] then MusAST.Minor 
                else MusAST.Major
            MusAST.Minor -> 
                if intervalVal == 1 then MusAST.Diminished 
                else if intervalVal `elem` [2,5,6] then MusAST.Major
                else MusAST.Minor
            _           -> error "Can't generate triad in invalid scale quality (i.e. not major or minor)"
    tone <- generateToneWithinScale tonicTone tonicQuality intervalVal specialOctaveCases octFunc
    triadList <- expandIntermediateExpr symbolTable (MusAST.ChordTemplate tone quality MusAST.Triad inversion duration) 
    return $ head triadList

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Expressions
-----------------------------------------------------------------------------------------
expandIntermediateExpr :: SymbolTable -> MusAST.IntermediateExpr -> IO [MusAST.Expr]

-- | Notes get expanded to become single-element chords
expandIntermediateExpr _ (MusAST.Note tone duration) = return [MusAST.Chord [tone] duration]

-- | Predefined chords
expandIntermediateExpr _ (MusAST.ChordTemplate (MusAST.Tone rootNoteName rootAccidental rootOctave) quality chordType inversion duration) = 
    if rootAccidental == MusAST.DoubleFlat || rootAccidental == MusAST.DoubleSharp then return [error "Cannot build chord on a double flat or sharp"]
    else if chordType == MusAST.Triad && quality == MusAST.HalfDiminished then return [error "Cannot have a half diminished triad"] else do
    let tonicTone = (MusAST.Tone rootNoteName rootAccidental rootOctave)
        toneQualityWithinScale = case quality of -- can only get tones within a valid (i.e. major/minor) scale
            MusAST.Major     -> MusAST.Major
            MusAST.Augmented -> MusAST.Major
            _                -> MusAST.Minor
        generateToneFromTonic = generateToneWithinScale tonicTone toneQualityWithinScale
    
    (MusAST.Tone thirdNoteName thirdAccidental thirdOctave) <- generateToneFromTonic 2 [MusAST.A, MusAST.B] succ
    (MusAST.Tone fifthNoteName fifthAccidental fifthOctave) <- generateToneFromTonic 4 (enumFromTo MusAST.F MusAST.B) succ 

    let adjustedFifthAcc = case quality of
            MusAST.Augmented      -> succ fifthAccidental
            MusAST.Diminished     -> pred fifthAccidental
            MusAST.HalfDiminished -> pred fifthAccidental
            _                     -> fifthAccidental 
        
        triadNoteNames   = [rootNoteName, thirdNoteName, fifthNoteName]
        triadAccidentals = [rootAccidental, thirdAccidental, adjustedFifthAcc]
        triadOctaves     = [rootOctave, thirdOctave, fifthOctave]
        inversionVal     = convertInversionToInt inversion
        
    if chordType == MusAST.Triad 
        then if inversionVal > 2 then return [error "Cannot have third inversion triad"] else 
            let invertedTriadOctaves = incrementFirstNElems inversionVal triadOctaves
                 -- NOTE: musescore doesn't care which note is on "top" of the chord in the musicXML: only that the correct notes are in the chord
                 -- thus, we don't need to rotate the array of triad tones to fit the inversion in the musicXML code
                invertedTriadTones = zipWith3 (\noteName accidental octave -> MusAST.Tone noteName accidental octave) triadNoteNames triadAccidentals invertedTriadOctaves
             in return [MusAST.Chord invertedTriadTones duration]
    else do
    (MusAST.Tone seventhNoteName seventhAccidental seventhOctave) <- generateToneFromTonic 6 (enumFromTo MusAST.F MusAST.B) succ 
    let adjustedSeventhAcc = case quality of
            MusAST.Augmented  -> pred seventhAccidental -- since augmented is generated w.r.t. major key
            MusAST.Diminished -> pred seventhAccidental -- since dim is generated w.r.t. minor key
            _                 -> seventhAccidental 
        
        invertedSeventhOctaves = incrementFirstNElems inversionVal (triadOctaves ++ [seventhOctave])
        invertedSeventhTones   = 
            zipWith3 (\noteName accidental octave -> MusAST.Tone noteName accidental octave) 
                (triadNoteNames ++ [seventhNoteName])
                (triadAccidentals ++ [adjustedSeventhAcc])
                invertedSeventhOctaves

    return [MusAST.Chord invertedSeventhTones duration]
       
-- | Quality is major/minor ONLY. tone+quality determines the start note and key of the cadence
expandIntermediateExpr symbolTable (MusAST.Cadence cadenceType (MusAST.Tone tonicNoteName tonicAccidental tonicOctave) quality duration) = 
    if quality `notElem` globalValidKeyQualities then return $ error "Cadence quality must be major or minor only" else do
    let tonicRootTone = MusAST.Tone tonicNoteName tonicAccidental tonicOctave
    tonicRootTriadList <- expandIntermediateExpr symbolTable (MusAST.ChordTemplate tonicRootTone quality MusAST.Triad MusAST.Root duration)
    let tonicRootTriad = head tonicRootTriadList
        generateTriad = generateTriadWithinScale symbolTable tonicRootTone quality duration 
    fourthSecondInvTriad <- generateTriad 3 (enumFromTo MusAST.C MusAST.F) pred MusAST.Second

    if cadenceType == MusAST.Plagal then return $ [fourthSecondInvTriad, tonicRootTriad] else do
    
    fourthRootTriad <- generateTriad 3 (enumFromTo MusAST.G MusAST.B) succ MusAST.Root
    fifthRootTriad <- generateTriadWithinScale symbolTable tonicRootTone MusAST.Major duration 4 (enumFromTo MusAST.F MusAST.B) succ MusAST.Root -- can't use generateTriad since V chord is always major no matter the key, in a cadence

    let tonicDoubledRootChord = MusAST.Chord (tonicRootTriadTones ++ doubledRootTone) duration
                where (MusAST.Chord tonicRootTriadTones _) = tonicRootTriad
                      doubledRootTone = [MusAST.Tone tonicNoteName tonicAccidental (succ tonicOctave)]
             
    if cadenceType == MusAST.PerfAuth then return $ [fourthRootTriad, fifthRootTriad, tonicDoubledRootChord] else do
    
    tonicFirstInvTriadList <- expandIntermediateExpr symbolTable (MusAST.ChordTemplate tonicRootTone quality MusAST.Triad MusAST.First duration)
    let tonicFirstInvTriad = head tonicFirstInvTriadList
    majSeventhSecondInvDimTriad <- generateTriadWithinScale symbolTable tonicRootTone MusAST.Major duration 6 [MusAST.C] pred MusAST.Second -- we want major seventh whether or not key is maj or min
        
    if cadenceType == MusAST.ImperfAuth then return $ [fourthRootTriad, majSeventhSecondInvDimTriad, tonicFirstInvTriad] else do
    
    sixthSecondInvTriad <- generateTriad 5 [MusAST.C, MusAST.D] pred MusAST.Second 

    -- For this to work, we want scale deg 5 BELOW scale deg one. Hence, lower octaves of fifths with tonic C thru E
    fifthSecondInvTriad <- generateTriad 4 (enumFromTo MusAST.C MusAST.E) pred MusAST.Second 

    if cadenceType == MusAST.Deceptive then return $ [fourthRootTriad, fifthSecondInvTriad, sixthSecondInvTriad] else do
    secondFirstInvTriad <- generateTriad 1 [MusAST.B] succ MusAST.First
    
    -- Half Cadence
    return $ [fourthRootTriad, secondFirstInvTriad, fifthRootTriad] 

-- | Quality is major/minor ONLY. tone+quality determines the start note and key of the harmseq
--   Duration is length of each chord, length is number of chords in the sequence
--   The seq chords all happen in root position
expandIntermediateExpr symbolTable (MusAST.HarmonicSequence harmSeqType tonicTone tonicQuality duration length) = 
    if tonicQuality `notElem` globalValidKeyQualities then return $ error "Harmonic Seq quality must be major or minor only" else 
    if length < 1 then return $ error "Harmonic Seq must have length at least 1 " else do 
    tonicRootTriadList <- expandIntermediateExpr symbolTable (MusAST.ChordTemplate tonicTone tonicQuality MusAST.Triad MusAST.Root duration) 
    tonicSecondInvTriadList <- expandIntermediateExpr symbolTable (MusAST.ChordTemplate tonicTone tonicQuality MusAST.Triad MusAST.Second duration) 

    let (MusAST.Tone tonicNoteName tonicAcc initialTonicOctave) = tonicTone
        tonicRootTriad = head tonicRootTriadList
        tonicSecondInvTriad = head tonicSecondInvTriadList
        
        cMajScaleNotesAsc = enumFromTo MusAST.C MusAST.B
        cMajScaleNotesDesc = reverse cMajScaleNotesAsc

        succ2 = succ . succ
        pred2 = pred . pred

        -- the logic of this is quite complicated and was worked out on paper
        specialOctCasesFunc nextIndexInSeq nextTonicOctave = case harmSeqType of 
            MusAST.AscFifths  ->
                if even nextIndexInSeq 
                    then (take (nextIndexInSeq `div` 2) cMajScaleNotesDesc, succ)
                else if nextIndexInSeq <= 7
                        then (take ((nextIndexInSeq - 1) `div` 2 + 4) cMajScaleNotesDesc, succ)
                else let (succ2Cases, succCases) = splitAt ((nextIndexInSeq - 7) `div` 2) cMajScaleNotesDesc
                        in if tonicNoteName `elem` succ2Cases then (succ2Cases, succ2) else (succCases, succ)
            MusAST.DescFifths ->
                if even nextIndexInSeq 
                    then (take (nextIndexInSeq `div` 2) cMajScaleNotesAsc, pred)
                else if nextIndexInSeq <= 5
                    then (take ((nextIndexInSeq + 7) `div` 2) cMajScaleNotesAsc, pred)
                else 
                    let (pred2Cases, predCases) = splitAt ((nextIndexInSeq - 7) `div` 2) cMajScaleNotesAsc
                    in if tonicNoteName `elem` pred2Cases then (pred2Cases, pred2) else (predCases, pred)
            MusAST.Asc56      ->
                if even nextIndexInSeq 
                    then (take (nextIndexInSeq `div` 2) cMajScaleNotesDesc, succ)
                else if nextIndexInSeq >= 7  
                    then (take ((nextIndexInSeq - 5) `div` 2) cMajScaleNotesDesc, succ)
                else if nextIndexInSeq <= 3 
                    then (take (if nextIndexInSeq == 1 then 2 else 1) cMajScaleNotesAsc, pred)
                -- const nextTonicOctave is placeholder for no oct func to apply for the index 5 chord in the seq (since this is just the tonic again)
                else ([], const nextTonicOctave)  
            MusAST.Desc56     ->
                if even nextIndexInSeq 
                    then if nextIndexInSeq <= 6 then (take nextIndexInSeq cMajScaleNotesAsc, pred)
                            else let (pred2Cases, predCases) = splitAt (nextIndexInSeq - 7) cMajScaleNotesAsc
                                in if tonicNoteName `elem` pred2Cases then (pred2Cases, pred2) else (predCases, pred)
                else if nextIndexInSeq == 13
                        then let (pred2Cases, predCases) = splitAt 1 cMajScaleNotesAsc
                            in if tonicNoteName `elem` pred2Cases then (pred2Cases, pred2) else (predCases, pred)
                else if nextIndexInSeq >= 7
                    then (take (nextIndexInSeq - 5) cMajScaleNotesAsc, pred)
                else if nextIndexInSeq <= 3
                    then (drop (nextIndexInSeq + 2) cMajScaleNotesAsc, succ)
                -- const nextTonicOctave is placeholder for no oct func to apply for the index 5 chord in the seq (since this is just the tonic again)
                else ([], const nextTonicOctave) 
                    
        -- the "index interval changes" tell you how to get to the next chord in the seq, from the current chord, via the interval separating them
        (tonicTriad, octIncVal, evenIndexIntervalChange, oddIndexIntervalChange, evenIndexInv, oddIndexInv) = case harmSeqType of 
            MusAST.Asc56      -> (tonicRootTriad, 1, 5, -4, MusAST.First, MusAST.Root)
            MusAST.Desc56     -> (tonicSecondInvTriad, -2, -3, 1, MusAST.Root, MusAST.Second)
            MusAST.AscFifths  -> (tonicSecondInvTriad, 1, 4, -3, MusAST.Root, MusAST.Second)
            MusAST.DescFifths -> (tonicRootTriad, -1, -4, 3, MusAST.Second, MusAST.Root)
 
        generateSeq 1 = return ([tonicTriad], 0, 0, initialTonicOctave) -- ([first chord], first interval from tonic, starting index in seq w/ 0-indexing, initial tonic octave)
        generateSeq n = do
            (remainingSeq, previousIntervalFromTonic, previousIndexInSeq, previousTonicOctave) <- generateSeq (n-1) 
            let nextIndexInSeq = (previousIndexInSeq + 1) `mod` 14 -- All sequences are 14 chords long
                nextTonicOctave = if nextIndexInSeq == 0 then previousTonicOctave + octIncVal else previousTonicOctave -- the octave changes every cycle of the seq
                nextTonicTone = (MusAST.Tone tonicNoteName tonicAcc nextTonicOctave)
                
                intervalFromTonic = (previousIntervalFromTonic + (if n `mod` 2 == 0 then evenIndexIntervalChange else oddIndexIntervalChange)) `mod` 7
                inversion = if n `mod` 2 == 0 then evenIndexInv else oddIndexInv
                
                (specialOctCasesForIndex, octFunc) = specialOctCasesFunc nextIndexInSeq nextTonicOctave

            triad <- generateTriadWithinScale symbolTable nextTonicTone tonicQuality duration intervalFromTonic specialOctCasesForIndex octFunc inversion
            return $ (remainingSeq ++ [triad], intervalFromTonic, nextIndexInSeq, nextTonicOctave) -- the seq cycles after 14 chords, but an octave up
        
    (finalSeq, _, _, _) <- generateSeq length
    return finalSeq

-- | Replace a label with its stored expressions
expandIntermediateExpr symbolTableIORef (MusAST.Label label) = do
    symbolTable <- IORef.readIORef symbolTableIORef
    let exprs = case Map.lookup label symbolTable of 
                    Just e -> e
                    Nothing -> error "label has been referenced before assignment to a musical expression"
    return [MusAST.LabeledExpr exprs]
                 
expandIntermediateExpr _ (MusAST.FinalExpr expr) = return [expr]

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstr :: SymbolTable -> MusAST.IntermediateInstr -> IO MusAST.Instr

-- | Quality is major/minor ONLY
expandIntermediateInstr symbolTableIORef (MusAST.IRKeySignature noteName accidental quality)
    | quality == MusAST.Major = 
        let convertSharpKeySig noteName = 
                let numSharpsMaybe = elemIndex (pred noteName) globalOrderOfSharps 
                in case numSharpsMaybe of
                    Just numSharps -> return $ MusAST.KeySignature (succ numSharps) 0 -- (succ numSharps) accounts for zero-indexing
                    Nothing -> return $ error "Cannot convert sharp key sig template"
        in case accidental of 
            MusAST.Flat -> 
                if noteName >= MusAST.C
                    then let sharpsIndexMaybe = elemIndex noteName globalOrderOfSharps
                            in case sharpsIndexMaybe of
                                -- this is a flat key signature
                                -- seven possible sharps or flats bc 7 possible note names
                                -- also, we will never have sharpsIndex go below 1 because F major is handled separately
                                Just sharpsIndex -> return $ MusAST.KeySignature 0 (7 - (sharpsIndex - 1)) 
                                Nothing          -> return $ error "Cannot convert flat key sig template"
                else return $ error "Key signature cannot have double flats"
            MusAST.Sharp -> 
                if noteName <= MusAST.C
                    then convertSharpKeySig noteName
                else return $ error "Key signature cannot have double flats"
            MusAST.Natural -> 
                case noteName of
                    MusAST.C -> return $ MusAST.KeySignature 0 0
                    MusAST.F -> return $ MusAST.KeySignature 0 1
                    otherNoteName -> convertSharpKeySig noteName -- this is a sharp key sig by definition
            _             -> return $ error "Cannot have key signature with double flats or sharps in key sig name" 
    | quality == MusAST.Minor =
        let majKeyNoteName = applyN succ noteName 2 -- we go up 3 semitones in the minor key to get the major key
            majAccidental = 
                case accidental of -- default is convert to major, then check if valid there, except when there's no single-accidentals major conversion
                    MusAST.Sharp -> 
                        if noteName >= MusAST.D then MusAST.Sharp
                        else MusAST.Natural
                    MusAST.Flat -> 
                        if noteName >= MusAST.D then MusAST.Flat 
                        else MusAST.DoubleFlat -- this is invalid by definition, will error once we recurse and re-process as relative major
                    MusAST.Natural -> 
                        if noteName >= MusAST.D then MusAST.Natural
                        else MusAST.Flat
        in expandIntermediateInstr symbolTableIORef (MusAST.IRKeySignature majKeyNoteName majAccidental MusAST.Major)
    | otherwise = return $ error "Key signature quality can only be major or minor"

expandIntermediateInstr _ MusAST.IRNewMeasure = return MusAST.NewMeasure

-- all uses of this label in IRWrite will get desugared in this file when they're referenced
expandIntermediateInstr symbolTable (MusAST.IRAssign label intermediateExprs)  = do
    expandedExprs <- concatMapM (expandIntermediateExpr symbolTable) intermediateExprs
    symbolTableUnpacked <- IORef.readIORef symbolTable
    IORef.writeIORef symbolTable (Map.insert label expandedExprs symbolTableUnpacked) -- store the label info in the symbol table
    return $ MusAST.Assign label expandedExprs

expandIntermediateInstr symbolTable (MusAST.IRWrite intermediateExprs)  = do
    exprs <- concatMapM (expandIntermediateExpr symbolTable) intermediateExprs
    return $ MusAST.Write exprs

-----------------------------------------------------------------------------------------
-- Expand All the Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstrs :: [MusAST.IntermediateInstr] -> IO [MusAST.Instr]
expandIntermediateInstrs intermediateInstrs = do
    let emptyTypedMap = Map.empty :: Map MusAST.Label [MusAST.Expr]
    symbolTableIORef <- IORef.newIORef emptyTypedMap
    mapM (expandIntermediateInstr symbolTableIORef) intermediateInstrs
