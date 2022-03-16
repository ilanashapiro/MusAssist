{-|
Module       : IRConversion
Description  : The intermediate representation for the compiler -- part of the middle. Expands chord/cadence/harmseq templates, etc
Maintainer   : Ilana Shapiro
-}

module IRConversion where

import qualified MusAssistAST         as MusAST
import           Data.List
import           Data.Either

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

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Expressions
-----------------------------------------------------------------------------------------
expandIntermediateExpr :: MusAST.IntermediateExpr -> IO MusAST.Expr

-- | Notes get expanded to become single-element chords
expandIntermediateExpr (MusAST.Note tone duration) = return $ MusAST.Chord [tone] duration

-- | Predefined chords
expandIntermediateExpr (MusAST.ChordTemplate (MusAST.Tone rootNoteName rootAccidental rootOctave) quality chordType inversion duration) = 
    if rootAccidental == MusAST.DoubleFlat || rootAccidental == MusAST.DoubleSharp then return $ error "Cannot build chord on a double flat or sharp" 
    else if chordType == MusAST.Triad && quality == MusAST.HalfDiminished then return $ error "Cannot have a half diminished triad" else do
    let generateIntervalAccidental noteName accidental cutoffNote = case accidental of
            MusAST.Flat    -> Left (if noteName >= cutoffNote then MusAST.Flat else MusAST.DoubleFlat)
            MusAST.Natural -> Left (if noteName >= cutoffNote then MusAST.Natural else MusAST.Flat)
            MusAST.Sharp   -> Left (if noteName >= cutoffNote then MusAST.Sharp else MusAST.Natural)
            _              -> Right "Cannot build interval from double flat or sharp"

        thirdOctave           = if rootNoteName `elem` [MusAST.A, MusAST.B] then rootOctave + 1 else rootOctave
        thirdNoteName         = applyN succ rootNoteName 2
        thirdAccidentalEither = 
            if quality `elem` [MusAST.Minor, MusAST.Diminished, MusAST.HalfDiminished] 
            then minorThirdAccidentalFromRoot
            else case minorThirdAccidentalFromRoot of -- Major and Augmented have major third root-third interval
                (Left accidental) -> Left (succ accidental)
                (Right error)     -> Right error
            where minorThirdAccidentalFromRoot = generateIntervalAccidental rootNoteName rootAccidental MusAST.D

    if isRight thirdAccidentalEither then return $ error (fromRight "Cannot generate chordal third accidental" thirdAccidentalEither) else do 
    let thirdAccidental       = head (lefts [thirdAccidentalEither]) 
        fifthOctave           = if rootNoteName `elem` [MusAST.C, MusAST.D, MusAST.E] then rootOctave else rootOctave + 1
        fifthNoteName         = applyN succ thirdNoteName 2
        fifthAccidentalEither = if rootNoteName == MusAST.B then Left rootAccidental else Left (succ rootAccidental)
    if isRight fifthAccidentalEither then return $ error (fromRight "Cannot generate chordal fifth accidental" fifthAccidentalEither) else do 
    let fifthAccidental  = head (lefts [fifthAccidentalEither]) 
        triadNoteNames   = [rootNoteName, thirdNoteName, fifthNoteName]
        triadAccidentals = [rootAccidental, thirdAccidental, fifthAccidental]
        triadOctaves     = [rootOctave, thirdOctave, fifthOctave]
        inversionVal     = convertInversionToInt inversion
        
    if chordType == MusAST.Triad 
        then if inversionVal > 2 then return $ error "Cannot have third inversion triad" else 
            let invertedTriadOctaves = incrementFirstNElems inversionVal triadOctaves
                 -- NOTE: musescore doesn't care which note is on "top" of the chord in the musicXML: only that the correct notes are in the chord
                 -- thus, we don't need to rotate the array of triad tones to fit the inversion in the musicXML code
                invertedTriadTones = zipWith3 (\noteName accidental octave -> MusAST.Tone noteName accidental octave) triadNoteNames triadAccidentals invertedTriadOctaves
             in return $ MusAST.Chord invertedTriadTones duration
    else do
    let seventhOctave           = if rootNoteName `elem` [MusAST.C, MusAST.D, MusAST.E] then rootOctave else rootOctave + 1
        seventhNoteName         = applyN succ fifthNoteName 2
        seventhAccidentalEither = 
            if quality `elem` [MusAST.Augmented, MusAST.Minor, MusAST.Diminished] 
            then minorSeventhAccidentalFromRoot
            else case minorSeventhAccidentalFromRoot of -- Major and Half-Dim seventh have major seventh root-seventh interval
                (Left accidental) -> Left (succ accidental)
                (Right error)     -> Right error
            where minorSeventhAccidentalFromRoot = generateIntervalAccidental rootNoteName rootAccidental MusAST.G

    if isRight seventhAccidentalEither then return $ error (fromRight "Cannot generate chordal seventh accidental" seventhAccidentalEither) else do 
    let seventhAccidental      = head (lefts [seventhAccidentalEither]) 
        invertedSeventhOctaves = incrementFirstNElems inversionVal (triadOctaves ++ [seventhOctave])
        invertedSeventhTones   = 
            zipWith3 (\noteName accidental octave -> MusAST.Tone noteName accidental octave) 
                (triadNoteNames ++ [seventhNoteName])
                (triadAccidentals ++ [seventhAccidental])
                invertedSeventhOctaves
    return $ MusAST.Chord invertedSeventhTones duration
       
-- | Quality is major/minor ONLY. tone+quality determines the start note and key of the cadence
expandIntermediateExpr (MusAST.Cadence cadenceType tone quality) = undefined

-- | Quality is major/minor ONLY. tone+quality determines the start note and key of the harmseq
--   Duration is length of each chord, length is number of chords in the sequence
--   The seq chords all happen in root position
expandIntermediateExpr (MusAST.HarmonicSequence harmSeqType tone quality duration length) = undefined

expandIntermediateExpr (MusAST.FinalExpr expr) = return expr

-----------------------------------------------------------------------------------------
-- Expand Individual Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstr :: MusAST.IntermediateInstr -> IO MusAST.Instr

-- | Quality is major/minor ONLY
expandIntermediateInstr (MusAST.SetKeySignature noteName accidental quality) 
    | quality == MusAST.Major = 
        let convertSharpKeySig noteName = 
                let numSharpsMaybe = elemIndex (pred noteName) globalOrderOfSharps 
                in case numSharpsMaybe of
                    Just numSharps -> return $ MusAST.KeySignature (numSharps + 1) 0 -- account for zero-indexing
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
    | quality == MusAST.Minor =
        let majKeyNoteName = applyN succ noteName 2 -- we go up 3 semitones in the minor key to get the major key
            majAccidentalMaybe = 
                case accidental of -- default is convert to major, then check if valid there, except when there's no single-accidentals major conversion
                    MusAST.Sharp -> 
                        if noteName >= MusAST.D then Just MusAST.Sharp
                        else Just MusAST.Natural
                    MusAST.Flat -> 
                        if noteName >= MusAST.D then Just MusAST.Flat -- technically, Db minor will error in Fb Major as double flats, but we don't error now for consistency 
                        else Nothing -- no conversion to major due to double flats
                    MusAST.Natural -> 
                        if noteName >= MusAST.D then Just MusAST.Natural
                        else Just MusAST.Flat
        in case majAccidentalMaybe of 
            Just majAccidental -> expandIntermediateInstr (MusAST.SetKeySignature majKeyNoteName majAccidental MusAST.Major)
            Nothing            -> return $ error "Cannot convert minor to major key sig due to double flats" 
    | otherwise = return $ error "Key signature quality can only be major or minor"

expandIntermediateInstr MusAST.CreateNewMeasure = return MusAST.NewMeasure

expandIntermediateInstr (MusAST.IRWrite intermediateExprs) = do
    exprs <- mapM expandIntermediateExpr intermediateExprs
    return $ MusAST.Write exprs

-----------------------------------------------------------------------------------------
-- Expand All the Intermediate Instructions
-----------------------------------------------------------------------------------------
expandIntermediateInstrs :: [MusAST.IntermediateInstr] -> IO [MusAST.Instr]
expandIntermediateInstrs intermediateInstrs = mapM expandIntermediateInstr intermediateInstrs